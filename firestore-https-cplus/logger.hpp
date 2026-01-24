#pragma once

#include <iostream>
#include <string_view>
#include <chrono>
#include <ctime>
#include <sstream>
#include <iomanip>
#include "json.hpp"

inline void log_json(std::string_view level, std::string_view msg,
              const nlohmann::json &data = nlohmann::json::object()) {
  auto now = std::chrono::system_clock::now();
  auto now_c = std::chrono::system_clock::to_time_t(now);
  std::tm now_tm{};
  gmtime_r(&now_c, &now_tm);

  std::ostringstream oss;
  oss << std::put_time(&now_tm, "%Y-%m-%dT%H:%M:%SZ");

  nlohmann::json j;
  j["timestamp"] = oss.str();
  j["level"] = level;
  j["message"] = msg;
  if (!data.empty()) {
    j["data"] = data;
  }

  std::cerr << j.dump() << std::endl;
}
