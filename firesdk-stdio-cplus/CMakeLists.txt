cmake_minimum_required(VERSION 3.10)
project(firesdk-stdio-cplus)

set(CMAKE_CXX_STANDARD 17)
add_definitions(-D_GLIBCXX_USE_CXX11_ABI=0)

# Enable SSL for cpp-mcp (required for Firestore HTTPS)
set(MCP_SSL ON CACHE BOOL "Enable SSL" FORCE)

# Firebase Setup
set(FIREBASE_INCLUDE_FIRESTORE ON CACHE BOOL "Enable Firestore" FORCE)
set(FIREBASE_FORCE_FAKE_SECURE_STORAGE ON CACHE BOOL "Disable platform secret store" FORCE)
add_subdirectory(firebase-cpp-sdk)

add_subdirectory(cpp-mcp)

include_directories(cpp-mcp/include)
include_directories(cpp-mcp/common)

add_executable(server main.cpp firestore_client.cpp)
target_link_libraries(server mcp firebase_firestore firebase_app)