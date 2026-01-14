#include <ctime>
#include <functional>
#include <iostream>
#include <map>
#include <mutex>
#include <string>
#include <vector>

#include "json.hpp"
#include "mcp_message.h"
#include "mcp_tool.h"

using json = mcp::json;

// JSON Logging Helper
void log_info(const std::string &msg) {
  std::time_t now = std::time(nullptr);
  char buf[30];
  std::strftime(buf, sizeof(buf), "%Y-%m-%dT%H:%M:%SZ", std::gmtime(&now));

  json j;
  j["asctime"] = buf;
  j["name"] = "root";
  j["levelname"] = "INFO";
  j["message"] = msg;

  std::cerr << j.dump() << std::endl;
}

class StdioServer {
public:
  StdioServer(const std::string &name, const std::string &version)
      : name_(name), version_(version) {}

  void register_tool(
      const mcp::tool &tool,
      std::function<json(const json &, const std::string &)> handler) {
    tools_[tool.name] = {tool, handler};
  }

  void start() {
    std::string line;
    while (std::getline(std::cin, line)) {
      try {
        if (line.empty())
          continue;

        // Parse request
        json j = json::parse(line);

        // Simple validation
        if (!j.contains("jsonrpc") || j["jsonrpc"] != "2.0") {
          // Ignore or error
        }

        // Ensure keys exist for mcp::request::from_json (which might throw on
        // const access)
        if (!j.contains("id"))
          j["id"] = nullptr;
        if (!j.contains("params"))
          j["params"] = nullptr;

        // Check if it's a request or notification
        // Using mcp::request helper
        mcp::request req = mcp::request::from_json(j);

        if (req.is_notification()) {
          handle_notification(req);
          continue;
        }

        // It's a request
        json response = handle_request(req);
        std::cout << response.dump() << std::endl;

      } catch (const std::exception &e) {
        // Try to report error if we can identify it as a request with ID
        // But for simplicity in this loop, we just log
        log_info("Error processing line: " + std::string(e.what()));
      }
    }
  }

private:
  std::string name_;
  std::string version_;
  std::map<std::string,
           std::pair<mcp::tool,
                     std::function<json(const json &, const std::string &)>>>
      tools_;
  bool initialized_ = false;

  void handle_notification(const mcp::request &req) {
    if (req.method == "notifications/initialized") {
      initialized_ = true;
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

      // For other methods, we technically should check initialized_, but
      // for simplicity and compatibility with some clients that might peek, we
      // can be lenient or strict. The spec says we should wait for initialized.
      if (!initialized_) {
        // However, we must return a response.
        return mcp::response::create_error(req.id,
                                           mcp::error_code::server_error_start,
                                           "Server not initialized")
            .to_json();
      }

      if (req.method == "tools/list") {
        json tools_list = json::array();
        for (const auto &pair : tools_) {
          tools_list.push_back(pair.second.first.to_json());
        }
        return mcp::response::create_success(req.id, {{"tools", tools_list}})
            .to_json();
      }

      if (req.method == "tools/call") {
        if (!req.params.contains("name")) {
          return mcp::response::create_error(req.id,
                                             mcp::error_code::invalid_params,
                                             "Missing 'name' parameter")
              .to_json();
        }

        std::string name = req.params["name"];
        if (tools_.find(name) == tools_.end()) {
          return mcp::response::create_error(req.id,
                                             mcp::error_code::method_not_found,
                                             "Tool not found: " + name)
              .to_json();
        }

        json args = req.params.contains("arguments") ? req.params["arguments"]
                                                     : json::object();

        try {
          // Call the tool handler
          json content = tools_[name].second(args, "stdio-session");

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

  mcp::response handle_initialize(const mcp::request &req) {
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

  // Define 'greet' tool
  mcp::tool greet_tool =
      mcp::tool_builder("greet")
          .with_description("Get a greeting from a local stdio server.")
          .with_string_param("param",
                             "Greeting parameter") // Note: The param name is
                                                   // 'param' in original C code
          .build();

  // Register handler
  server.register_tool(
      greet_tool,
      [](const json &args, const std::string & /* session_id */) -> json {
        log_info("Executed greet tool");

        std::string param;
        if (args.contains("param")) {
          param = args["param"].get<std::string>();
        } else {
          // Should probably not happen if schema is respected, but good to be
          // safe
          param = "";
        }

        // Return content array
        return json::array({{
            {"type", "text"},
            {"text", param} // The original C code returns just the param string
        }});
      });

  server.start();

  return 0;
}
