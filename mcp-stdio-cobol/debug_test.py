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
        print("Sending initialize...")
        process.stdin.write(json.dumps(init_request) + "\n")
        process.stdin.flush()
        
        # Read initialize response
        line = process.stdout.readline()
        print("STDOUT:", line.strip())
        
        # Send initialized notification
        initialized_notification = {
            "jsonrpc": "2.0",
            "method": "notifications/initialized"
        }
        process.stdin.write(json.dumps(initialized_notification) + "\n")
        process.stdin.flush()

        # Send greet call
        print("Sending greet...")
        process.stdin.write(json.dumps(greet_request) + "\n")
        process.stdin.flush()

        # Read greet response
        line = process.stdout.readline()
        print("STDOUT:", line.strip())
        
        # Check stderr
        stderr_output = process.stderr.read()
        if stderr_output:
            print("STDERR:\n", stderr_output)

    except Exception as e:
        print("Error:", e)
    finally:
        process.terminate()

if __name__ == "__main__":
    test_greet()
