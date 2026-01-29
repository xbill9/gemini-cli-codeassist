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
                print("✓ Greet successful")
            else:
                print("Test FAILED: Unexpected response content for greet")
                sys.exit(1)

        # 4. List tools
        print("Listing tools...")
        list_request = {
            "jsonrpc": "2.0",
            "id": 3,
            "method": "tools/list",
            "params": {}
        }
        req = urllib.request.Request(url, data=json.dumps(list_request).encode('utf-8'), headers={'Content-Type': 'application/json'})
        with urllib.request.urlopen(req) as response:
            resp_data = json.loads(response.read().decode('utf-8'))
            tools = [t['name'] for t in resp_data.get('result', {}).get('tools', [])]
            print("Tools found:", tools)
            required_tools = ['greet', 'check_db', 'get_root', 'get_products', 'get_product_by_id', 'seed', 'reset']
            for t in required_tools:
                if t not in tools:
                    print(f"Test FAILED: Tool {t} not found in tools/list")
                    sys.exit(1)
            print("✓ tools/list successful")

        # 5. Check DB
        print("Sending check_db call...")
        check_db_request = {
            "jsonrpc": "2.0",
            "id": 4,
            "method": "tools/call",
            "params": {"name": "check_db", "arguments": {}}
        }
        req = urllib.request.Request(url, data=json.dumps(check_db_request).encode('utf-8'), headers={'Content-Type': 'application/json'})
        with urllib.request.urlopen(req) as response:
            resp_data = json.loads(response.read().decode('utf-8'))
            text = resp_data.get("result", {}).get("content", [])[0].get("text", "")
            print("check_db response:", text)
            if "Database running:" in text:
                print("✓ check_db successful")
            else:
                print("Test FAILED: check_db failed")
                sys.exit(1)

        # 6. Get Root
        print("Sending get_root call...")
        get_root_request = {
            "jsonrpc": "2.0",
            "id": 5,
            "method": "tools/call",
            "params": {"name": "get_root", "arguments": {}}
        }
        req = urllib.request.Request(url, data=json.dumps(get_root_request).encode('utf-8'), headers={'Content-Type': 'application/json'})
        with urllib.request.urlopen(req) as response:
            resp_data = json.loads(response.read().decode('utf-8'))
            text = resp_data.get("result", {}).get("content", [])[0].get("text", "")
            if "Cymbal Superstore Inventory API" in text:
                print("✓ get_root successful")
            else:
                print("Test FAILED: get_root failed")
                sys.exit(1)

        # 7. Get Products
        print("Sending get_products call...")
        get_products_request = {
            "jsonrpc": "2.0",
            "id": 6,
            "method": "tools/call",
            "params": {"name": "get_products", "arguments": {}}
        }
        req = urllib.request.Request(url, data=json.dumps(get_products_request).encode('utf-8'), headers={'Content-Type': 'application/json'})
        with urllib.request.urlopen(req) as response:
            resp_data = json.loads(response.read().decode('utf-8'))
            content = resp_data.get("result", {}).get("content", [])
            if content:
                text = content[0].get("text", "")
                print(f"get_products response: {text[:100]}...")
                if text.startswith("["):
                    print("✓ get_products successful")
                else:
                    print("Test FAILED: get_products didn't return a JSON array")
                    sys.exit(1)
            else:
                print("Test FAILED: get_products returned no content")
                sys.exit(1)

        print("All tests PASSED")

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
