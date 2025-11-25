# mcp-stdio-rust

A Rust application that interacts via standard input/output (stdio) using the MCP protocol.

## Overview

This project implements a command-line application in Rust that communicates through standard input and standard output. It's designed to process incoming commands or data from stdin and produce responses or results to stdout, adhering to a defined protocol.

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
target/release/mcp-stdio-rust
```
Then type your input and press Enter. The application will print its response to stdout.

## Protocol Details

This impliments a minimially viable MCP stdio server in Rust.
