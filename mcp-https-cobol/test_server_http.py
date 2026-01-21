import json
import subprocess
import sys
import time
import urllib.request
import urllib.error
import os
import signal

def run_test():
    port = "8081"
    env = os.environ.copy()
    env["PORT"] = port
    
    # Start the server
    print(f"Starting server on port {port}...")
    process = subprocess.Popen(
        ['./server-cobol'],
        env=env,
        stderr=subprocess.PIPE,
        text=True
    )
    
    # Wait for server to start
    time.sleep(2)
    
    url = f"http://localhost:{port}/mcp" # Endpoint path doesn't strictly matter based on server.c implementation, but using /mcp is standard-ish

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
        # 1. Send initialize
        print("Sending initialize...")
        req = urllib.request.Request(url, data=json.dumps(init_request).encode('utf-8'), headers={'Content-Type': 'application/json'})
        with urllib.request.urlopen(req) as response:
            resp_data = json.loads(response.read().decode('utf-8'))
            print("Initialize response:", json.dumps(resp_data, indent=2))

        # 2. Send initialized notification
        # Notifications don't expect a response, but HTTP usually returns 200 OK with empty body or similar.
        # Based on server.c: sends "HTTP/1.1 200 OK... Content-Length: 0" for noreply.
        print("Sending initialized notification...")
        initialized_notification = {
            "jsonrpc": "2.0",
            "method": "notifications/initialized"
        }
        req = urllib.request.Request(url, data=json.dumps(initialized_notification).encode('utf-8'), headers={'Content-Type': 'application/json'})
        with urllib.request.urlopen(req) as response:
            print(f"Initialized notification status: {response.status}")

        # 3. Send greet call
        print("Sending greet call...")
        req = urllib.request.Request(url, data=json.dumps(greet_request).encode('utf-8'), headers={'Content-Type': 'application/json'})
        with urllib.request.urlopen(req) as response:
            resp_data = json.loads(response.read().decode('utf-8'))
            print("Greet response:", json.dumps(resp_data, indent=2))
            
            # Verify response
            result = resp_data.get("result", {})
            content = result.get("content", [])
            if content and content[0].get("text") == "Hello, World!":
                print("Test PASSED")
            else:
                print("Test FAILED: Unexpected response content")
                sys.exit(1)

    except urllib.error.URLError as e:
        print(f"Connection failed: {e}")
        # Print stderr to see if server crashed
        if process.poll() is not None:
             print("Server process exited prematurely.")
             print(process.stderr.read())
        sys.exit(1)
    except Exception as e:
        print(f"An error occurred: {e}")
        sys.exit(1)
    finally:
        print("Terminating server...")
        process.terminate()
        try:
            process.wait(timeout=5)
        except subprocess.TimeoutExpired:
            process.kill()
        
        # Read remaining stderr
        stderr_output = process.stderr.read()
        if stderr_output:
            print("Server Stderr:")
            print(stderr_output)

if __name__ == "__main__":
    run_test()
