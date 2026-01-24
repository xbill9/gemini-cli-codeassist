#pragma once

#include "json.hpp"
#include <optional>
#include <string>
#include <vector>

class FirestoreClient {
public:
  FirestoreClient();
  ~FirestoreClient() = default;

  bool initialize();
  bool is_initialized() const { return initialized_; }

  nlohmann::json get_products();
  nlohmann::json get_product(const std::string &id);
  nlohmann::json search_products(const std::string &query);
  void seed();
  void reset();

private:
  std::string project_id_;
  std::string access_token_;
  bool initialized_ = false;

  std::string get_access_token();
  std::string get_project_id();
  void ensure_authenticated();

  // Helper to perform HTTP requests
  // Returns the JSON response body
  nlohmann::json make_firestore_request(const std::string &method,
                                        const std::string &path,
                                        const nlohmann::json &body = {});

  // Converters
  nlohmann::json doc_to_product(const nlohmann::json &doc);
  nlohmann::json product_to_doc(const nlohmann::json &product);

  void add_or_update_product(const nlohmann::json &product);
};
