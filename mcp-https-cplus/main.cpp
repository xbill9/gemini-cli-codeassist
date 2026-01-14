#include <chrono>
#include <ctime>
#include <filesystem>
#include <functional>
#include <iomanip>
#include <iostream>
#include <map>
#include <mutex>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

#include "mcp_server.h"
#include "mcp_tool.h"

// JSON Logging Helper
void log_json(std::string_view level, std::string_view msg,
              const mcp::json &data = mcp::json::object()) {
  auto now = std::chrono::system_clock::now();
  auto now_c = std::chrono::system_clock::to_time_t(now);
  std::tm now_tm{};
  gmtime_r(&now_c, &now_tm);

  std::ostringstream oss;
  oss << std::put_time(&now_tm, "%Y-%m-%dT%H:%M:%SZ");

  mcp::json j;
  j["timestamp"] = oss.str();
  j["level"] = level;
  j["message"] = msg;
  if (!data.empty()) {
    j["data"] = data;
  }

  std::cerr << j.dump() << std::endl;
}

int main() {
  try {
    // Read port from environment variable, default to 8080
    int port = 8080;
    const char *port_env = std::getenv("PORT");
    if (port_env) {
      try {
        port = std::stoi(port_env);
      } catch (...) {
        log_json("WARNING", "Invalid PORT environment variable, using default",
                 {{"port", port}});
      }
    }

    // Create and configure server
    mcp::server::configuration srv_conf;
    srv_conf.host = "0.0.0.0";
    srv_conf.port = port;

    mcp::server server(srv_conf);
    server.set_server_info("mcp-https-cplus", "1.0.0");

    // Set server capabilities
    mcp::json capabilities = {{"tools", mcp::json::object()}};
    server.set_capabilities(capabilities);

    // Register tools
    server.register_tool(
        mcp::tool_builder("greet")
            .with_description("Get a greeting from a local https server.")
            .with_string_param("param", "Greeting parameter")
            .build(),
        [](const mcp::json &args,
           const std::string & /* session_id */) -> mcp::json {
          log_json("INFO", "Executed greet tool");
          std::string param = args.value("param", "");

          return mcp::json::array({{{"type", "text"}, {"text", param}}});
        });

    log_json("INFO", "Starting MCP HTTP server",
             {{"host", srv_conf.host}, {"port", srv_conf.port}});
    server.start(true); // Blocking mode

  } catch (const std::exception &e) {
    log_json("ERROR", "Fatal exception", {{"error", e.what()}});
    return 1;
  } catch (...) {
    log_json("ERROR", "Unknown fatal exception");
    return 1;
  }

  return 0;
}
