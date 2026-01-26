#pragma once

#include <string>
#include <vector>
#include <map>
#include "json.hpp"
#include "firebase/app.h"
#include "firebase/firestore.h"

namespace Firestore {

using json = nlohmann::json;

class Client {
public:
    Client(const std::string &project);
    ~Client();
    
    // Legacy support (no-op in SDK implementation usually)
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
    firebase::App* app_;
    firebase::firestore::Firestore* db_;

    // Helpers
    json DocumentToJSON(const firebase::firestore::DocumentSnapshot& doc);
    firebase::firestore::MapFieldValue JsonToFields(const json& j);
    firebase::firestore::FieldValue JsonValueToFieldValue(const json& j);
    json FieldValueToJson(const firebase::firestore::FieldValue& v);
};

} // namespace Firestore