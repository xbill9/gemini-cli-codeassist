import json
import subprocess
import sys

def test_greet():
    process = subprocess.Popen(
        ['./server-cobol'],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True
    )

    # Initialize request
    init_request = {
        "jsonrpc": "2.0",
        "id": 1,
        "method": "initialize",
        "params": {
            "protocolVersion": "2024-11-05",
            "capabilities": {},
            "clientInfo": {"name": "test-client", "version": "1.0.0"}
        }
    }

    # Call tool request
    greet_request = {
        "jsonrpc": "2.0",
        "id": 2,
        "method": "tools/call",
        "params": {
            "name": "greet",
            "arguments": {"param": "World"}
        }
    }

    try:
        # Send initialize
        process.stdin.write(json.dumps(init_request) + "\n")
        process.stdin.flush()
        
        # Read initialize response (and skip possible logs on stderr)
        line = process.stdout.readline()
        if not line:
            print("Failed to read initialize response")
            sys.exit(1)
        
        init_response = json.loads(line)
        print("Initialize response received")

        # Send initialized notification
        initialized_notification = {
            "jsonrpc": "2.0",
            "method": "notifications/initialized"
        }
        process.stdin.write(json.dumps(initialized_notification) + "\n")
        process.stdin.flush()

        # Send greet call
        process.stdin.write(json.dumps(greet_request) + "\n")
        process.stdin.flush()

        # Read greet response
        line = process.stdout.readline()
        if not line:
            print("Failed to read greet response")
            sys.exit(1)
            
        greet_response = json.loads(line)
        print("Greet response received:", json.dumps(greet_response, indent=2))

        # Verify response
        result = greet_response.get("result", {})
        content = result.get("content", [])
        if content and content[0].get("text") == "Hello, World!":
            print("Test PASSED")
        else:
            print("Test FAILED: Unexpected response content")
            sys.exit(1)

    finally:
        process.terminate()

if __name__ == "__main__":
    test_greet()
