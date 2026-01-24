#include <algorithm>
#include <chrono>
#include <cstdio>
#include <cstdlib>
#include <ctime>
#include <functional>
#include <iomanip>
#include <iostream>
#include <map>
#include <mutex>
#include <random>
#include <sstream>
#include <string>
#include <thread>
#include <vector>

#define CPPHTTPLIB_OPENSSL_SUPPORT
#include "httplib.h"
#include "json.hpp"
#include "mcp_message.h"
#include "mcp_tool.h"

using json = nlohmann::json;

// Global Configuration
std::string PROJECT_ID;
std::string ACCESS_TOKEN;

// JSON Logging Helper
void log_json(const std::string &level, const std::string &msg,
              const json &data = json::object()) {
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

// Authentication Helper
std::string GetAccessToken() {
  const char *env_token = std::getenv("FIRESTORE_ACCESS_TOKEN");
  if (env_token)
    return std::string(env_token);

  // Try gcloud
  std::string token;
  FILE *pipe = popen("gcloud auth print-access-token", "r");
  if (!pipe) {
    log_json("WARN", "Failed to run gcloud to get access token");
    return "";
  }
  char buffer[128];
  while (fgets(buffer, sizeof(buffer), pipe) != NULL) {
    token += buffer;
  }
  pclose(pipe);
  token.erase(token.find_last_not_of(" \n\r\t") + 1);

  if (token.empty()) {
    log_json("WARN", "gcloud returned empty token");
  }
  return token;
}

std::string GetProjectId() {
  const char *env_p = std::getenv("GOOGLE_CLOUD_PROJECT");
  if (env_p)
    return std::string(env_p);

  const char *env_f = std::getenv("FIREBASE_PROJECT_ID");
  if (env_f)
    return std::string(env_f);

  // Try gcloud
  std::string pid;
  FILE *pipe = popen("gcloud config get-value project", "r");
  if (pipe) {
    char buffer[128];
    while (fgets(buffer, sizeof(buffer), pipe) != NULL) {
      pid += buffer;
    }
    pclose(pipe);
    pid.erase(pid.find_last_not_of(" \n\r\t") + 1);
  }
  return pid;
}

// Firestore REST Helpers
namespace Firestore {

// Convert Simple JSON to Firestore Value
json ToValue(const json &j) {
  if (j.is_null())
    return {{"nullValue", nullptr}};
  if (j.is_boolean())
    return {{"booleanValue", j.get<bool>()}};
  if (j.is_number_integer())
    return {{"integerValue", std::to_string(j.get<long long>())}};
  if (j.is_number_float())
    return {{"doubleValue", j.get<double>()}};
  if (j.is_string())
    return {{"stringValue", j.get<std::string>()}};
  if (j.is_array()) {
    json arr = json::array();
    for (const auto &el : j)
      arr.push_back(ToValue(el));
    return {{"arrayValue", {{"values", arr}}}};
  }
  if (j.is_object()) {
    // Special case for Timestamp if encoded as object in our logic
    if (j.contains("type") && j["type"] == "timestamp" && j.contains("value")) {
      return {{"timestampValue", j["value"]}};
    }

    json fields = json::object();
    for (auto &[key, val] : j.items()) {
      fields[key] = ToValue(val);
    }
    return {{"mapValue", {{"fields", fields}}}};
  }
  return {{"nullValue", nullptr}};
}

// Convert Firestore Value to Simple JSON
json FromValue(const json &v) {
  if (v.contains("nullValue"))
    return nullptr;
  if (v.contains("booleanValue"))
    return v["booleanValue"];
  if (v.contains("integerValue"))
    return std::stoll(v["integerValue"].get<std::string>());
  if (v.contains("doubleValue"))
    return v["doubleValue"];
  if (v.contains("stringValue"))
    return v["stringValue"];
  if (v.contains("timestampValue"))
    return v["timestampValue"];
  if (v.contains("mapValue")) {
    json obj = json::object();
    if (v["mapValue"].contains("fields")) {
      for (auto &[key, val] : v["mapValue"]["fields"].items()) {
        obj[key] = FromValue(val);
      }
    }
    return obj;
  }
  if (v.contains("arrayValue")) {
    json arr = json::array();
    if (v["arrayValue"].contains("values")) {
      for (const auto &el : v["arrayValue"]["values"]) {
        arr.push_back(FromValue(el));
      }
    }
    return arr;
  }
  return nullptr;
}

// Parse Document
json ParseDocument(const json &doc) {
  if (!doc.contains("name"))
    return json::object(); // invalid
  json obj = json::object();

  // Extract ID from name
  std::string name = doc["name"];
  size_t lastSlash = name.find_last_of('/');
  if (lastSlash != std::string::npos) {
    obj["id"] = name.substr(lastSlash + 1);
  }

  if (doc.contains("fields")) {
    for (auto &[key, val] : doc["fields"].items()) {
      obj[key] = FromValue(val);
    }
  }
  return obj;
}

class Client {
public:
  Client(const std::string &project)
      : project_(project), cli_("https://firestore.googleapis.com") {
    cli_.set_connection_timeout(10);
    cli_.set_read_timeout(10);
  }

  void SetToken(const std::string &token) { token_ = token; }

  bool CheckConnection() {
    // Try getting database metadata as a ping
    auto res =
        cli_.Get(("/v1/projects/" + project_ + "/databases/(default)").c_str(),
                 GetHeaders());
    return res && res->status == 200;
  }

  json ListDocuments(const std::string &collection) {
    std::string path = "/v1/projects/" + project_ +
                       "/databases/(default)/documents/" + collection;
    auto res = cli_.Get(path.c_str(), GetHeaders());
    if (!res || res->status != 200) {
      throw std::runtime_error(
          "Failed to list documents: " +
          (res ? std::to_string(res->status) : "Connection Error"));
    }
    json j = json::parse(res->body);
    json results = json::array();
    if (j.contains("documents")) {
      for (const auto &doc : j["documents"]) {
        results.push_back(ParseDocument(doc));
      }
    }
    return results;
  }

  json GetDocument(const std::string &collection, const std::string &id) {
    std::string path = "/v1/projects/" + project_ +
                       "/databases/(default)/documents/" + collection + "/" +
                       id;
    auto res = cli_.Get(path.c_str(), GetHeaders());
    if (!res)
      throw std::runtime_error("Connection error");
    if (res->status == 404)
      throw std::runtime_error("Document not found");
    if (res->status != 200)
      throw std::runtime_error("Error getting document: " +
                               std::to_string(res->status));

    return ParseDocument(json::parse(res->body));
  }

  void AddDocument(const std::string &collection, const json &data,
                   const std::string &id = "") {
    std::string path = "/v1/projects/" + project_ +
                       "/databases/(default)/documents/" + collection;
    std::string params = "";
    if (!id.empty()) {
      params = "?documentId=" + id;
    }

    json doc;
    json fields = json::object();
    for (auto &[key, val] : data.items()) {
      fields[key] = ToValue(val);
    }
    doc["fields"] = fields;

    auto res = cli_.Post((path + params).c_str(), GetHeaders(), doc.dump(),
                         "application/json");
    if (!res || res->status != 200) {
      throw std::runtime_error("Failed to add document: " +
                               (res ? res->body : "Connection Error"));
    }
  }

  void UpdateDocument(const std::string &collection, const std::string &id,
                      const json &data) {
    std::string path = "/v1/projects/" + project_ +
                       "/databases/(default)/documents/" + collection + "/" +
                       id;
    // Use PATCH
    json doc;
    json fields = json::object();
    for (auto &[key, val] : data.items()) {
      fields[key] = ToValue(val);
    }
    doc["fields"] = fields;

    auto res =
        cli_.Patch(path.c_str(), GetHeaders(), doc.dump(), "application/json");
    if (!res || res->status != 200) {
      throw std::runtime_error("Failed to update document: " +
                               (res ? res->body : "Connection Error"));
    }
  }

  void DeleteDocument(const std::string &collection, const std::string &id) {
    std::string path = "/v1/projects/" + project_ +
                       "/databases/(default)/documents/" + collection + "/" +
                       id;
    auto res = cli_.Delete(path.c_str(), GetHeaders());
    if (!res || res->status != 200) {
      throw std::runtime_error(
          "Failed to delete document: " +
          (res ? std::to_string(res->status) : "Connection Error"));
    }
  }

  // Returns list of documents (simplified) that match the query
  json RunQuery(const std::string &collection, const std::string &field,
                const std::string &value) {
    std::string path =
        "/v1/projects/" + project_ + "/databases/(default)/documents:runQuery";
    json query = {{"structuredQuery",
                   {{"from", {{{"collectionId", collection}}}},
                    {"where",
                     {{"fieldFilter",
                       {{"field", {{"fieldPath", field}}},
                        {"op", "EQUAL"},
                        {"value", {{"stringValue", value}}}}}}}}}};

    auto res =
        cli_.Post(path.c_str(), GetHeaders(), query.dump(), "application/json");
    if (!res || res->status != 200) {
      throw std::runtime_error("Query failed: " +
                               (res ? res->body : "Connection Error"));
    }

    json response = json::parse(res->body);
    json results = json::array();
    if (response.is_array()) {
      for (const auto &item : response) {
        if (item.contains("document")) {
          results.push_back(ParseDocument(item["document"]));
        }
      }
    }
    return results;
  }

private:
  std::string project_;
  std::string token_;
  httplib::Client cli_;

  httplib::Headers GetHeaders() {
    httplib::Headers headers;
    if (!token_.empty()) {
      headers.emplace("Authorization", "Bearer " + token_);
    }
    return headers;
  }
};

} // namespace Firestore

// Server State
bool db_running = false;
std::unique_ptr<Firestore::Client> db_client;

// Helper to seed
void InitFirestoreCollection() {
  std::vector<std::string> oldProducts = {
      "Apples",       "Bananas",         "Milk",          "Whole Wheat Bread",
      "Eggs",         "Cheddar Cheese",  "Whole Chicken", "Rice",
      "Black Beans",  "Bottled Water",   "Apple Juice",   "Cola",
      "Coffee Beans", "Green Tea",       "Watermelon",    "Broccoli",
      "Jasmine Rice", "Yogurt",          "Beef",          "Shrimp",
      "Walnuts",      "Sunflower Seeds", "Fresh Basil",   "Cinnamon",
  };

  std::random_device rd;
  std::mt19937 gen(rd());
  std::uniform_int_distribution<> price_dist(1, 10);
  std::uniform_int_distribution<> qty_dist_500(1, 500);
  std::uniform_int_distribution<> qty_dist_100(1, 100);
  auto now = std::chrono::system_clock::now();

  auto seed_product = [&](const std::string &productName,
                          long long time_offset_ms, int qty) {
    auto past_time = now - std::chrono::milliseconds(time_offset_ms);
    std::string imgfile = "product-images/";
    std::string lowerName = productName;
    std::transform(lowerName.begin(), lowerName.end(), lowerName.begin(),
                   [](unsigned char c) { return std::tolower(c); });
    lowerName.erase(std::remove(lowerName.begin(), lowerName.end(), ' '),
                    lowerName.end());
    imgfile += lowerName + ".png";

    // Format timestamps for REST (ISO string)
    auto to_iso = [](std::chrono::system_clock::time_point tp) {
      auto t = std::chrono::system_clock::to_time_t(tp);
      std::tm tm{};
      gmtime_r(&t, &tm);
      std::ostringstream oss;
      oss << std::put_time(&tm, "%Y-%m-%dT%H:%M:%SZ");
      return oss.str();
    };

    json product;
    product["name"] = productName;
    product["price"] = price_dist(gen);
    product["quantity"] = qty;
    product["imgfile"] = imgfile;
    product["timestamp"] = {{"type", "timestamp"},
                            {"value", to_iso(past_time)}};
    product["actualdateadded"] = {{"type", "timestamp"},
                                  {"value", to_iso(now)}};

    // Check if exists
    try {
      auto existing = db_client->RunQuery("inventory", "name", productName);
      if (existing.empty()) {
        db_client->AddDocument("inventory", product);
        log_json("INFO", "Added product: " + productName);
      } else {
        std::string id = existing[0]["id"];
        db_client->UpdateDocument("inventory", id, product);
        log_json("INFO", "Updated product: " + productName);
      }
    } catch (const std::exception &e) {
      log_json("ERROR", "Failed to seed product " + productName,
               {{"error", e.what()}});
    }
  };

  for (const auto &p : oldProducts) {
    std::uniform_int_distribution<long long> time_dist(7776000000, 39312000000);
    seed_product(p, time_dist(gen), qty_dist_500(gen));
  }

  std::vector<std::string> recentProducts = {
      "Parmesan Crisps", "Pineapple Kombucha", "Maple Almond Butter"};
  for (const auto &p : recentProducts) {
    std::uniform_int_distribution<long long> time_dist(0, 518400000);
    seed_product(p, time_dist(gen), qty_dist_100(gen));
  }
}

void CleanFirestoreCollection() {
  try {
    auto docs = db_client->ListDocuments("inventory");
    for (const auto &doc : docs) {
      db_client->DeleteDocument("inventory", doc["id"]);
    }
    log_json("INFO", "Collection cleaned");
  } catch (const std::exception &e) {
    log_json("ERROR", "Failed to clean collection", {{"error", e.what()}});
    throw;
  }
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
        log_json("DEBUG", "Received message",
                 {{"content",
                   line}}); // Corrected: Added missing closing brace and comma
        json j = json::parse(line);

        if (!j.contains("jsonrpc") || j["jsonrpc"] != "2.0") {
          continue;
        }

        if (!j.contains("id"))
          j["id"] = nullptr;
        if (!j.contains("params"))
          j["params"] = json::object();

        mcp::request req = mcp::request::from_json(j);

        if (req.is_notification()) {
          handle_notification(req);
        } else {
          json response = handle_request(req);
          log_json(
              "DEBUG", "Sending response",
              {{"content",
                response}}); // Corrected: Added missing closing brace and comma
          std::cout << response.dump() << std::endl;
        }

      } catch (const std::exception &e) {
        log_json(
            "ERROR", "Error processing line",
            {{"error",
              e.what()}}); // Corrected: Added missing closing brace and comma
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
          tools_list.push_back(json(entry.definition.to_json()));
        }
        return mcp::response::create_success(req.id, {{"tools", tools_list}})
            .to_json();
      }
      if (req.method == "tools/call") {
        return handle_tool_call(req);
      }
      return mcp::response::create_error(
                 req.id, mcp::error_code::method_not_found, "Method not found")
          .to_json();
    } catch (const std::exception &e) {
      return mcp::response::create_error(
                 req.id, mcp::error_code::internal_error, e.what())
          .to_json();
    }
  }

  json handle_tool_call(const mcp::request &req) {
    std::string name = req.params["name"];
    auto it = tools_.find(name);
    if (it == tools_.end()) {
      return mcp::response::create_error(
                 req.id, mcp::error_code::method_not_found, "Tool not found")
          .to_json();
    }
    const json &args = req.params.value("arguments", json::object());
    try {
      json content = it->second.handler(args, "stdio-session");
      return mcp::response::create_success(
                 req.id, {{"content", content}, {"isError", false}})
          .to_json();
    } catch (const std::exception &e) {
      json content = json::array();
      content.push_back({{"type", "text"}, {"text", e.what()}});
      return mcp::response::create_success(
                 req.id, {{"content", content}, {"isError", true}})
          .to_json();
    }
  }

  mcp::response handle_initialize(const mcp::request &req) const {
    json result;
    result["protocolVersion"] = "2024-11-05";
    result["capabilities"] = {{"tools", {{"listChanged", false}}}};
    result["serverInfo"] = {{"name", name_}, {"version", version_}};

    return mcp::response::create_success(req.id, result);
  }
};

int main() {
  std::setvbuf(stdout, nullptr, _IONBF, 0);

  PROJECT_ID = GetProjectId();
  ACCESS_TOKEN = GetAccessToken();

  if (!PROJECT_ID.empty()) {
    log_json("INFO", "Project ID: " + PROJECT_ID);
    db_client = std::make_unique<Firestore::Client>(PROJECT_ID);
    if (!ACCESS_TOKEN.empty()) {
      db_client->SetToken(ACCESS_TOKEN);
      log_json("INFO", "Access token configured");
    } else {
      log_json("WARN", "No access token found. Requests might fail if not "
                       "using emulator or public dataset.");
    }

    if (db_client->CheckConnection()) {
      log_json("INFO", "Firestore connection successful");
      db_running = true;
    } else {
      log_json("ERROR", "Firestore connection failed");
    }

  } else {
    log_json("ERROR", "Project ID not found. Set GOOGLE_CLOUD_PROJECT or "
                      "FIREBASE_PROJECT_ID.");
  }

  StdioServer server("inventory-server", "1.0.0");

  server.register_tool(
      mcp::tool_builder("greet").with_string_param("param", "Name").build(),
      [](const json &args, const std::string &) -> json {
        return json::array(
            {{{"type", "text"}, {"text", args.value("param", "")}}});
      });

  server.register_tool(
      mcp::tool_builder("reverse_string")
          .with_description("Reverse a string")
          .with_string_param("input", "The string to reverse")
          .build(),
      [](const json &args, const std::string &) -> json {
        std::string str = args.value("input", "");
        std::reverse(str.begin(), str.end());
        return json::array({{{"type", "text"}, {"text", str}}});
      });

  server.register_tool(
      mcp::tool_builder("get_products")
          .with_description("Get all products")
          .build(),
      [](const json &, const std::string &) -> json {
        if (!db_running)
          throw std::runtime_error("DB not connected");
        auto products = db_client->ListDocuments("inventory");
        return json::array({{{"type", "text"}, {"text", products.dump(2)}}});
      });

  server.register_tool(
      mcp::tool_builder("get_product_by_id")
          .with_string_param("id", "Product ID")
          .build(),
      [](const json &args, const std::string &) -> json {
        if (!db_running)
          throw std::runtime_error("DB not connected");
        auto product =
            db_client->GetDocument("inventory", args.value("id", ""));
        return json::array({{{"type", "text"}, {"text", product.dump(2)}}});
      });

  server.register_tool(
      mcp::tool_builder("seed").with_description("Seed DB").build(),
      [](const json &, const std::string &) -> json {
        if (!db_running)
          throw std::runtime_error("DB not connected");
        InitFirestoreCollection();
        return json::array({{{"type", "text"}, {"text", "Seeded"}}});
      });

  server.register_tool(
      mcp::tool_builder("reset").with_description("Clear DB").build(),
      [](const json &, const std::string &) -> json {
        if (!db_running)
          throw std::runtime_error("DB not connected");
        CleanFirestoreCollection();
        return json::array({{{"type", "text"}, {"text", "Reset"}}});
      });

  server.register_tool(
      mcp::tool_builder("check_db").build(),
      [](const json &, const std::string &) -> json {
        return json::array(
            {{{"type", "text"},
              {"text", db_running ? "Connected" : "Not connected"}}});
      });

  server.register_tool(
      mcp::tool_builder("get_root").build(),
      [](const json &, const std::string &) -> json {
        return json::array(
            {{{"type", "text"},
              {"text",
               "🍎 Hello! This is the Cymbal Superstore Inventory API."}}});
      });

  server.start();
  return 0;
}
