#include "firestore_client.hpp"
#include <iostream>

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

Client::Client(const std::string &project)
    : project_(project), cli_("https://firestore.googleapis.com") {
  cli_.set_connection_timeout(10);
  cli_.set_read_timeout(10);
}

void Client::SetToken(const std::string &token) { token_ = token; }

bool Client::CheckConnection() {
  // Try getting database metadata as a ping
  auto res =
      cli_.Get(("/v1/projects/" + project_ + "/databases/(default)").c_str(),
               GetHeaders());
  return res && res->status == 200;
}

json Client::ListDocuments(const std::string &collection) {
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

json Client::GetDocument(const std::string &collection, const std::string &id) {
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

void Client::AddDocument(const std::string &collection, const json &data,
                 const std::string &id) {
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

void Client::UpdateDocument(const std::string &collection, const std::string &id,
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

void Client::DeleteDocument(const std::string &collection, const std::string &id) {
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
json Client::RunQuery(const std::string &collection, const std::string &field,
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

httplib::Headers Client::GetHeaders() {
  httplib::Headers headers;
  if (!token_.empty()) {
    headers.emplace("Authorization", "Bearer " + token_);
  }
  return headers;
}

} // namespace Firestore
