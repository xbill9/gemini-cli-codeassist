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

#include "firestore_client.hpp"
#include "logger.hpp"
#include "mcp_server.h"
#include "mcp_tool.h"

int main() {
  try {
    std::cerr << "Starting main..." << std::endl;
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

    std::cerr << "Initializing FirestoreClient..." << std::endl;
    // Initialize Firestore Client
    auto db = std::make_shared<FirestoreClient>();
    std::cerr << "Calling db->initialize()..." << std::endl;
    bool db_running = db->initialize();

    if (db_running) {
      log_json("INFO", "Firestore connected successfully.");
    } else {
      log_json("ERROR", "Error connecting to Firestore.");
    }

    std::cerr << "Configuring MCP server..." << std::endl;
    // Create and configure server
    mcp::server::configuration srv_conf;
    srv_conf.host = "0.0.0.0";
    srv_conf.port = port;

    std::cerr << "Creating mcp::server instance..." << std::endl;
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

    server.register_tool(
        mcp::tool_builder("get_products")
            .with_description(
                "Get a list of all products from the inventory database")
            .build(),
        [db](const mcp::json &, const std::string &) -> mcp::json {
          if (!db->is_initialized())
            throw std::runtime_error("Inventory database is not running.");
          try {
            auto products = db->get_products();
            return mcp::json::array(
                {{{"type", "text"}, {"text", products.dump(2)}}});
          } catch (const std::exception &e) {
            log_json("ERROR", "Error fetching products", {{"error", e.what()}});
            throw std::runtime_error("Error fetching products.");
          }
        });

    server.register_tool(
        mcp::tool_builder("get_product_by_id")
            .with_description(
                "Get a single product from the inventory database by its ID")
            .with_string_param("id", "The ID of the product to get")
            .build(),
        [db](const mcp::json &args, const std::string &) -> mcp::json {
          if (!db->is_initialized())
            throw std::runtime_error("Inventory database is not running.");
          std::string id = args.value("id", "");
          if (id.empty())
            throw std::runtime_error("Missing product ID.");

          try {
            auto product = db->get_product(id);
            // If product is empty/null, it means not found or error
            if (product.is_null() || product.empty()) {
              throw std::runtime_error("Product not found.");
            }
            return mcp::json::array(
                {{{"type", "text"}, {"text", product.dump(2)}}});
          } catch (const std::exception &e) {
            log_json("ERROR", "Error fetching product",
                     {{"id", id}, {"error", e.what()}});
            throw; // Rethrow to let server handle it as error
          }
        });

    server.register_tool(
        mcp::tool_builder("search")
            .with_description(
                "Search for products in the inventory database by name")
            .with_string_param("query",
                               "The search query to filter products by name")
            .build(),
        [db](const mcp::json &args, const std::string &) -> mcp::json {
          if (!db->is_initialized())
            throw std::runtime_error("Inventory database is not running.");
          std::string query = args.value("query", "");

          try {
            auto results = db->search_products(query);
            return mcp::json::array(
                {{{"type", "text"}, {"text", results.dump(2)}}});
          } catch (const std::exception &e) {
            log_json("ERROR", "Error searching products",
                     {{"error", e.what()}});
            throw std::runtime_error("Error searching products.");
          }
        });

    server.register_tool(
        mcp::tool_builder("seed")
            .with_description("Seed the inventory database with products.")
            .build(),
        [db](const mcp::json &, const std::string &) -> mcp::json {
          if (!db->is_initialized())
            throw std::runtime_error("Inventory database is not running.");
          try {
            db->seed();
            return mcp::json::array(
                {{{"type", "text"},
                  {"text", "Database seeded successfully."}}});
          } catch (const std::exception &e) {
            log_json("ERROR", "Error seeding database", {{"error", e.what()}});
            throw std::runtime_error("Error seeding database.");
          }
        });

    server.register_tool(
        mcp::tool_builder("reset")
            .with_description("Clear all products from the inventory database.")
            .build(),
        [db](const mcp::json &, const std::string &) -> mcp::json {
          if (!db->is_initialized())
            throw std::runtime_error("Inventory database is not running.");
          try {
            db->reset();
            return mcp::json::array(
                {{{"type", "text"}, {"text", "Database reset successfully."}}});
          } catch (const std::exception &e) {
            log_json("ERROR", "Error resetting database",
                     {{"error", e.what()}});
            throw std::runtime_error("Error resetting database.");
          }
        });

    server.register_tool(
        mcp::tool_builder("get_root")
            .with_description(
                "Get a greeting from the Cymbal Superstore Inventory API.")
            .build(),
        [](const mcp::json &, const std::string &) -> mcp::json {
          return mcp::json::array(
              {{{"type", "text"},
                {"text",
                 "🍎 Hello! This is the Cymbal Superstore Inventory API."}}});
        });

    log_json("INFO", "Starting MCP HTTP server",
             {{"host", srv_conf.host},
              {"port", srv_conf.port},
              {"db_running", db_running}});
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
