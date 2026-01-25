#pragma once

#include <string>
#include <vector>
#include "json.hpp"
#define CPPHTTPLIB_OPENSSL_SUPPORT
#include "httplib.h"

namespace Firestore {

using json = nlohmann::json;

class Client {
public:
    Client(const std::string &project);
    void SetToken(const std::string &token);
    bool CheckConnection();
    json ListDocuments(const std::string &collection);
    json GetDocument(const std::string &collection, const std::string &id);
    void AddDocument(const std::string &collection, const json &data, const std::string &id = "");
    void UpdateDocument(const std::string &collection, const std::string &id, const json &data);
    void DeleteDocument(const std::string &collection, const std::string &id);
    json RunQuery(const std::string &collection, const std::string &field, const std::string &value);

private:
    std::string project_;
    std::string token_;
    httplib::Client cli_;

    httplib::Headers GetHeaders();
};

} // namespace Firestore
