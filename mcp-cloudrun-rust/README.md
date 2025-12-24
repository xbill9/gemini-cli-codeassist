# mcp-https-rust

A Rust application that interacts via streaming http using the MCP protocol.

## Overview

## Getting Started

### Prerequisites

Ensure you have the Rust toolchain installed:

*   [Rust Toolchain](https://www.rust-lang.org/tools/install)

### Build

To build the project:

```bash
cargo build --release
```

### Run

To run the application, you can pipe input to it and capture its output:

```bash
echo "your_input_here" | target/release/mcp-stdio-rust
```

Or, for interactive use:

```bash
target/release/mcp-https-rust
```
Then type your input and press Enter. The application will print its response to stdout.

## Protocol Details

This impliments a minimially viable MCP streaming http server in Rust.
