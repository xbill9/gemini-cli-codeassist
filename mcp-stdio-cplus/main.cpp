#include <chrono>
#include <ctime>
#include <functional>
#include <iomanip>
#include <iostream>
#include <map>
#include <mutex>
#include <sstream>
#include <string>
#include <vector>

#include "json.hpp"
#include "mcp_message.h"
#include "mcp_tool.h"

using json = mcp::json;

// JSON Logging Helper
void log_json(const std::string &level, const std::string &msg, const json &data = json::object()) {
  auto now = std::chrono::system_clock::now();
  auto now_c = std::chrono::system_clock::to_time_t(now);
  std::tm now_tm{};
  gmtime_r(&now_c, &now_tm);

  std::ostringstream oss;
  oss << std::put_time(&now_tm, "%Y-%m-%dT%H:%M:%SZ");

  json j;
  j["timestamp"] = oss.str();
  j["level"] = level;
  j["message"] = msg;
  if (!data.empty()) {
    j["data"] = data;
  }

  std::cerr << j.dump() << std::endl;
}

class StdioServer {
public:
  struct ToolEntry {
    mcp::tool definition;
    std::function<json(const json &, const std::string &)> handler;
  };

  StdioServer(const std::string &name, const std::string &version)
      : name_(name), version_(version) {}

  void register_tool(
      const mcp::tool &tool,
      std::function<json(const json &, const std::string &)> handler) {
    tools_[tool.name] = {tool, std::move(handler)};
  }

  void start() {
    log_json("INFO", "Server starting...");
    std::string line;
    while (std::getline(std::cin, line)) {
      if (line.empty())
        continue;

      try {
        log_json("DEBUG", "Received message", {{"content", line}});
        json j = json::parse(line);

        // Simple JSON-RPC validation
        if (!j.contains("jsonrpc") || j["jsonrpc"] != "2.0") {
          log_json("WARN", "Invalid JSON-RPC version", {{"content", j}});
          continue;
        }

        // Ensure keys exist for mcp::request::from_json which expects them
        if (!j.contains("id"))
          j["id"] = nullptr;
        if (!j.contains("params"))
          j["params"] = json::object();

        mcp::request req = mcp::request::from_json(j);

        if (req.is_notification()) {
          handle_notification(req);
        } else {
          json response = handle_request(req);
          log_json("DEBUG", "Sending response", {{"content", response}});
          std::cout << response.dump() << std::endl;
        }

      } catch (const std::exception &e) {
        log_json("ERROR", "Error processing line", {{"error", e.what()}});
      }
    }
  }

private:
  std::string name_;
  std::string version_;
  std::map<std::string, ToolEntry> tools_;
  bool initialized_ = false;

  void handle_notification(const mcp::request &req) {
    log_json("INFO", "Received notification", {{"method", req.method}});
    if (req.method == "notifications/initialized") {
      initialized_ = true;
      log_json("INFO", "Server initialized");
    }
  }

  json handle_request(const mcp::request &req) {
    log_json("INFO", "Received request", {{"method", req.method}, {"id", req.id}});
    try {
      if (req.method == "initialize") {
        log_json("INFO", "Initializing server...");
        return handle_initialize(req).to_json();
      }

      if (req.method == "ping") {
        log_json("DEBUG", "Ping received");
        return mcp::response::create_success(req.id, json::object()).to_json();
      }

      if (!initialized_) {
        log_json("WARN", "Request received before initialization", {{"method", req.method}});
        return mcp::response::create_error(req.id,
                                           mcp::error_code::server_error_start,
                                           "Server not initialized")
            .to_json();
      }

      if (req.method == "tools/list") {
        log_json("INFO", "Listing tools");
        json tools_list = json::array();
        for (const auto &[name, entry] : tools_) {
          tools_list.push_back(entry.definition.to_json());
        }
        return mcp::response::create_success(req.id, {{"tools", tools_list}})
            .to_json();
      }

      if (req.method == "tools/call") {
        return handle_tool_call(req);
      }

      log_json("WARN", "Method not found", {{"method", req.method}});
      return mcp::response::create_error(req.id,
                                         mcp::error_code::method_not_found,
                                         "Method not found: " + req.method)
          .to_json();

    } catch (const std::exception &e) {
      log_json("ERROR", "Internal error handling request", {{"error", e.what()}});
      return mcp::response::create_error(
                 req.id, mcp::error_code::internal_error, e.what())
          .to_json();
    }
  }

  json handle_tool_call(const mcp::request &req) {
    if (!req.params.contains("name")) {
      log_json("WARN", "Tool call missing name parameter");
      return mcp::response::create_error(req.id, mcp::error_code::invalid_params,
                                         "Missing 'name' parameter")
          .to_json();
    }

    std::string name = req.params["name"];
    auto it = tools_.find(name);
    if (it == tools_.end()) {
      log_json("WARN", "Tool not found", {{"name", name}});
      return mcp::response::create_error(req.id,
                                         mcp::error_code::method_not_found,
                                         "Tool not found: " + name)
          .to_json();
    }

    const json &args = req.params.value("arguments", json::object());
    log_json("INFO", "Executing tool", {{"name", name}, {"arguments", args}});

    try {
      json content = it->second.handler(args, "stdio-session");
      log_json("INFO", "Tool execution successful", {{"name", name}});
      return mcp::response::create_success(
                 req.id, {{"content", content}, {"isError", false}})
          .to_json();
    } catch (const std::exception &e) {
      log_json("ERROR", "Tool execution failed", {{"name", name}, {"error", e.what()}});
      return mcp::response::create_success(
                 req.id,
                 {{"content", {{{"type", "text"}, {"text", e.what()}}}},
                  {"isError", true}})
          .to_json();
    }
  }

  mcp::response handle_initialize(const mcp::request &req) const {
    json result = {{"protocolVersion", "2024-11-05"},
                   {"capabilities", {{"tools", {{"listChanged", false}}}}},
                   {"serverInfo", {{"name", name_}, {"version", version_}}}};
    return mcp::response::create_success(req.id, result);
  }
};

int main() {
  // Ensure stdout is unbuffered
  std::setvbuf(stdout, nullptr, _IONBF, 0);

  StdioServer server("mcp-stdio-cplus", "1.0.0");

  server.register_tool(
      mcp::tool_builder("greet")
          .with_description("Get a greeting from a local stdio server.")
          .with_string_param("param", "Greeting parameter")
          .build(),
      [](const json &args, const std::string & /* session_id */) -> json {
        log_json("INFO", "Executed greet tool");
        std::string param = args.value("param", "");

        return json::array({{{"type", "text"}, {"text", param}}});
      });

  server.start();

  return 0;
}
