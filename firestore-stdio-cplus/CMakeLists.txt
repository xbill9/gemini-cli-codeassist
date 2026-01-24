cmake_minimum_required(VERSION 3.10)
project(firestore-stdio-cplus)

set(CMAKE_CXX_STANDARD 17)

# Enable SSL for cpp-mcp (required for Firestore HTTPS)
set(MCP_SSL ON CACHE BOOL "Enable SSL" FORCE)

add_subdirectory(cpp-mcp)

include_directories(cpp-mcp/include)
include_directories(cpp-mcp/common)

add_executable(server main.cpp)
target_link_libraries(server mcp)
