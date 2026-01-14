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
void log_info(const std::string &msg) {
  auto now = std::chrono::system_clock::now();
  auto now_c = std::chrono::system_clock::to_time_t(now);
  std::tm now_tm{};
  gmtime_r(&now_c, &now_tm);

  std::ostringstream oss;
  oss << std::put_time(&now_tm, "%Y-%m-%dT%H:%M:%SZ");

  json j;
  j["asctime"] = oss.str();
  j["name"] = "root";
  j["levelname"] = "INFO";
  j["message"] = msg;

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
    std::string line;
    while (std::getline(std::cin, line)) {
      if (line.empty())
        continue;

      try {
        json j = json::parse(line);

        // Simple JSON-RPC validation
        if (!j.contains("jsonrpc") || j["jsonrpc"] != "2.0") {
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
          std::cout << response.dump() << std::endl;
        }

      } catch (const std::exception &e) {
        log_info("Error processing line: " + std::string(e.what()));
      }
    }
  }

private:
  std::string name_;
  std::string version_;
  std::map<std::string, ToolEntry> tools_;
  bool initialized_ = false;

  void handle_notification(const mcp::request &req) {
    if (req.method == "notifications/initialized") {
      initialized_ = true;
      log_info("Server initialized");
    }
  }

  json handle_request(const mcp::request &req) {
    try {
      if (req.method == "initialize") {
        return handle_initialize(req).to_json();
      }

      if (req.method == "ping") {
        return mcp::response::create_success(req.id, json::object()).to_json();
      }

      if (!initialized_) {
        return mcp::response::create_error(req.id,
                                           mcp::error_code::server_error_start,
                                           "Server not initialized")
            .to_json();
      }

      if (req.method == "tools/list") {
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

      return mcp::response::create_error(req.id,
                                         mcp::error_code::method_not_found,
                                         "Method not found: " + req.method)
          .to_json();

    } catch (const std::exception &e) {
      return mcp::response::create_error(
                 req.id, mcp::error_code::internal_error, e.what())
          .to_json();
    }
  }

  json handle_tool_call(const mcp::request &req) {
    if (!req.params.contains("name")) {
      return mcp::response::create_error(req.id, mcp::error_code::invalid_params,
                                         "Missing 'name' parameter")
          .to_json();
    }

    std::string name = req.params["name"];
    auto it = tools_.find(name);
    if (it == tools_.end()) {
      return mcp::response::create_error(req.id,
                                         mcp::error_code::method_not_found,
                                         "Tool not found: " + name)
          .to_json();
    }

    const json &args = req.params.value("arguments", json::object());

    try {
      json content = it->second.handler(args, "stdio-session");
      return mcp::response::create_success(
                 req.id, {{"content", content}, {"isError", false}})
          .to_json();
    } catch (const std::exception &e) {
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
        log_info("Executed greet tool");
        std::string param = args.value("param", "");

        return json::array({{{"type", "text"}, {"text", param}}});
      });

  server.start();

  return 0;
}
