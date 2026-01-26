#include "firestore_client.hpp"
#include <iostream>
#include <thread>
#include <chrono>
#include <sstream>
#include <iomanip>
#include <cstdio>
#include <cstdlib>
#include <sys/stat.h>
#include "firebase/util.h"

namespace Firestore {

// Helper to wait for futures
template <typename T>
const T* WaitForFuture(const firebase::Future<T>& future) {
    while (future.status() == firebase::kFutureStatusPending) {
        std::this_thread::sleep_for(std::chrono::milliseconds(10));
    }
    if (future.status() != firebase::kFutureStatusComplete) {
        throw std::runtime_error("Future failed to complete");
    }
    if (future.error() != 0) {
        throw std::runtime_error(std::string("Firebase Error: ") + future.error_message());
    }
    return future.result();
}

// Specialization for void futures
void WaitForFuture(const firebase::Future<void>& future) {
    while (future.status() == firebase::kFutureStatusPending) {
        std::this_thread::sleep_for(std::chrono::milliseconds(10));
    }
    if (future.status() != firebase::kFutureStatusComplete) {
        throw std::runtime_error("Future failed to complete");
    }
    if (future.error() != 0) {
        throw std::runtime_error(std::string("Firebase Error: ") + future.error_message());
    }
}

static bool FileExists(const std::string& name) {
  struct stat buffer;   
  return (stat(name.c_str(), &buffer) == 0); 
}

Client::Client(const std::string &project) {
    // ADC Setup
    if (std::getenv("GOOGLE_APPLICATION_CREDENTIALS") == nullptr) {
        const char* home = std::getenv("HOME");
        if (home) {
            std::string adc_path = std::string(home) + "/.config/gcloud/application_default_credentials.json";
            if (FileExists(adc_path)) {
                setenv("GOOGLE_APPLICATION_CREDENTIALS", adc_path.c_str(), 1);
            }
        }
    }

    firebase::AppOptions options;
    options.set_project_id(project.c_str());
    
    // On Desktop, the SDK requires an API Key and App ID to pass validation,
    // even if Firestore ends up using ADC. 
    // We use placeholders if not set via environment.
    options.set_api_key("AIzaSyDummyKeyForADCSupport");
    options.set_app_id("1:1234567890:android:dummyappid");

    std::cerr << "{\"level\":\"INFO\",\"message\":\"Creating Firebase App for project: " << project << "\",\"timestamp\":\"\"}" << std::endl;
    
    app_ = firebase::App::Create(options);
    if (!app_) {
        throw std::runtime_error("Failed to create Firebase App");
    }

    db_ = firebase::firestore::Firestore::GetInstance(app_);
    if (!db_) {
        throw std::runtime_error("Failed to get Firestore instance");
    }
    
    db_->set_log_level(firebase::LogLevel::kLogLevelInfo);
}

Client::~Client() {
    if (db_) {
        delete db_; 
        db_ = nullptr;
    }
    if (app_) {
        delete app_;
        app_ = nullptr;
    }
}

void Client::SetToken(const std::string &token) {
    // No-op
}

bool Client::CheckConnection() {
    try {
        auto future = db_->Collection("inventory").Limit(1).Get();
        WaitForFuture(future);
        return true;
    } catch (const std::exception& e) {
        std::cerr << "{\"level\":\"ERROR\",\"message\":\"CheckConnection failed: " << e.what() << "\",\"timestamp\":\"\"}" << std::endl;
        return false;
    } catch (...) {
        std::cerr << "{\"level\":\"ERROR\",\"message\":\"CheckConnection failed with unknown error\",\"timestamp\":\"\"}" << std::endl;
        return false;
    }
}

json Client::ListDocuments(const std::string &collection) {
    auto future = db_->Collection(collection).Get();
    const auto* result = WaitForFuture(future);
    
    json list = json::array();
    for (const auto& doc : result->documents()) {
        list.push_back(DocumentToJSON(doc));
    }
    return list;
}

json Client::GetDocument(const std::string &collection, const std::string &id) {
    auto future = db_->Collection(collection).Document(id).Get();
    const auto* result = WaitForFuture(future);
    if (!result->exists()) {
        throw std::runtime_error("Document not found");
    }
    return DocumentToJSON(*result);
}

void Client::AddDocument(const std::string &collection, const json &data, const std::string &id) {
    auto fields = JsonToFields(data);
    if (!id.empty()) {
        auto future = db_->Collection(collection).Document(id).Set(fields);
        WaitForFuture(future);
    } else {
        auto future = db_->Collection(collection).Add(fields);
        WaitForFuture(future);
    }
}

void Client::UpdateDocument(const std::string &collection, const std::string &id, const json &data) {
    auto fields = JsonToFields(data);
    auto future = db_->Collection(collection).Document(id).Update(fields);
    WaitForFuture(future);
}

void Client::DeleteDocument(const std::string &collection, const std::string &id) {
    auto future = db_->Collection(collection).Document(id).Delete();
    WaitForFuture(future);
}

json Client::RunQuery(const std::string &collection, const std::string &field, const std::string &value) {
    auto future = db_->Collection(collection).WhereEqualTo(field, firebase::firestore::FieldValue::String(value)).Get();
    const auto* result = WaitForFuture(future);
    
    json list = json::array();
    for (const auto& doc : result->documents()) {
        list.push_back(DocumentToJSON(doc));
    }
    return list;
}

// Helpers

firebase::firestore::FieldValue Client::JsonValueToFieldValue(const json& j) {
    if (j.is_null()) return firebase::firestore::FieldValue::Null();
    if (j.is_boolean()) return firebase::firestore::FieldValue::Boolean(j.get<bool>());
    if (j.is_number_integer()) return firebase::firestore::FieldValue::Integer(j.get<int64_t>());
    if (j.is_number_float()) return firebase::firestore::FieldValue::Double(j.get<double>());
    if (j.is_string()) return firebase::firestore::FieldValue::String(j.get<std::string>());
    
    if (j.is_array()) {
        std::vector<firebase::firestore::FieldValue> arr;
        for (const auto& el : j) {
            arr.push_back(JsonValueToFieldValue(el));
        }
        return firebase::firestore::FieldValue::Array(arr);
    }
    
    if (j.is_object()) {
        if (j.contains("type") && j["type"] == "timestamp" && j.contains("value")) {
            std::string iso_time = j["value"];
            std::tm tm = {};
            std::istringstream ss(iso_time);
            ss >> std::get_time(&tm, "%Y-%m-%dT%H:%M:%SZ");
            if (!ss.fail()) {
                 time_t t = timegm(&tm);
                 return firebase::firestore::FieldValue::Timestamp({t, 0});
            }
        }

        firebase::firestore::MapFieldValue map;
        for (auto& [key, val] : j.items()) {
            map[key] = JsonValueToFieldValue(val);
        }
        return firebase::firestore::FieldValue::Map(map);
    }
    
    return firebase::firestore::FieldValue::Null();
}

firebase::firestore::MapFieldValue Client::JsonToFields(const json& j) {
    firebase::firestore::MapFieldValue fields;
    if (!j.is_object()) return fields;
    for (auto& [key, val] : j.items()) {
        fields[key] = JsonValueToFieldValue(val);
    }
    return fields;
}

json Client::FieldValueToJson(const firebase::firestore::FieldValue& v) {
    if (v.is_null()) return nullptr;
    if (v.is_boolean()) return v.boolean_value();
    if (v.is_integer()) return v.integer_value();
    if (v.is_double()) return v.double_value();
    if (v.is_string()) return v.string_value();
    
    if (v.is_timestamp()) {
        auto ts = v.timestamp_value();
        std::time_t t = ts.seconds();
        std::tm tm;
        gmtime_r(&t, &tm);
        std::ostringstream oss;
        oss << std::put_time(&tm, "%Y-%m-%dT%H:%M:%SZ");
        
        return {
            {"type", "timestamp"},
            {"value", oss.str()}
        };
    }
    
    if (v.is_array()) {
        json arr = json::array();
        for (const auto& el : v.array_value()) {
            arr.push_back(FieldValueToJson(el));
        }
        return arr;
    }
    
    if (v.is_map()) {
        json obj = json::object();
        for (const auto& [key, val] : v.map_value()) {
            obj[key] = FieldValueToJson(val);
        }
        return obj;
    }
    
    return nullptr;
}

json Client::DocumentToJSON(const firebase::firestore::DocumentSnapshot& doc) {
    json j = json::object();
    j["id"] = doc.id();
    auto data = doc.GetData();
    for (const auto& [key, val] : data) {
        j[key] = FieldValueToJson(val);
    }
    return j;
}

} // namespace Firestore