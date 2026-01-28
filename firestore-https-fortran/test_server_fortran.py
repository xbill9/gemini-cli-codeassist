import json
import subprocess
import sys
import time
import urllib.request
import urllib.error
import os

def run_test():
    port = "8080"
    env = os.environ.copy()
    env["PORT"] = port
    
    # Start the server
    print(f"Starting server on port {port}...")
    # Binary name is currently firestore-server
    process = subprocess.Popen(
        ['./firestore-https-server'],
        env=env,
        stderr=subprocess.PIPE,
        text=True
    )
    
    # Wait for server to start
    time.sleep(2)
    
    url = f"http://localhost:{port}/mcp"

    try:
        # 1. Initialize
        print("Sending initialize...")
        init_req = {
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {
                "protocolVersion": "2024-11-05",
                "capabilities": {},
                "clientInfo": {"name": "test-client", "version": "1.0"}
            }
        }
        
        req = urllib.request.Request(url, data=json.dumps(init_req).encode('utf-8'), headers={'Content-Type': 'application/json'})
        with urllib.request.urlopen(req) as response:
            resp_data = json.loads(response.read().decode('utf-8'))
            if "result" in resp_data:
                print("✓ initialize successful")
            else:
                print(f"Initialize failed: {resp_data}")
                sys.exit(1)

        # 2. Initialized notification
        print("Sending initialized notification...")
        initialized_notif = {
            "jsonrpc": "2.0",
            "method": "notifications/initialized"
        }
        req = urllib.request.Request(url, data=json.dumps(initialized_notif).encode('utf-8'), headers={'Content-Type': 'application/json'})
        with urllib.request.urlopen(req) as response:
            print("✓ notifications/initialized sent")

        # 3. List tools
        print("Sending tools/list...")
        list_req = {
            "jsonrpc": "2.0",
            "id": 2,
            "method": "tools/list",
            "params": {}
        }
        req = urllib.request.Request(url, data=json.dumps(list_req).encode('utf-8'), headers={'Content-Type': 'application/json'})
        with urllib.request.urlopen(req) as response:
            res = json.loads(response.read().decode('utf-8'))
            tools = [t['name'] for t in res.get('result', {}).get('tools', [])]
            required_tools = ['greet', 'check_db', 'get_root', 'get_products', 'get_product_by_id', 'seed', 'reset']
            missing_tools = [t for t in required_tools if t not in tools]
            
            if missing_tools:
                print(f"Error: Missing tools: {missing_tools}. Found: {tools}")
                print(f"Full response: {json.dumps(res, indent=2)}")
                sys.exit(1)
            print("✓ tools/list successful (all tools found)")

        # 4. Call greet
        print("Testing 'greet' tool...")
        call_req = {
            "jsonrpc": "2.0",
            "id": 3,
            "method": "tools/call",
            "params": {
                "name": "greet",
                "arguments": {
                    "param": "Gemini"
                }
            }
        }
        req = urllib.request.Request(url, data=json.dumps(call_req).encode('utf-8'), headers={'Content-Type': 'application/json'})
        with urllib.request.urlopen(req) as response:
            res = json.loads(response.read().decode('utf-8'))
            content = res.get('result', {}).get('content', [])
            text = content[0].get('text') if content else ""
            if text == "Hello, Gemini!":
                print("✓ tools/call (greet) successful")
            else:
                print(f"Error: Unexpected response for greet: {text}")
                sys.exit(1)

        # 5. Call check_db
        print("Testing 'check_db' tool...")
        check_db_req = {
            "jsonrpc": "2.0",
            "id": 4,
            "method": "tools/call",
            "params": {"name": "check_db", "arguments": {}}
        }
        req = urllib.request.Request(url, data=json.dumps(check_db_req).encode('utf-8'), headers={'Content-Type': 'application/json'})
        with urllib.request.urlopen(req) as response:
            res = json.loads(response.read().decode('utf-8'))
            text = res.get('result', {}).get('content', [])[0].get('text', '')
            if "Database running:" in text:
                print(f"✓ tools/call (check_db) successful: {text}")
            else:
                 print(f"Error: Unexpected check_db response: {text}")
                 sys.exit(1)

        # 6. Call get_root
        print("Testing 'get_root' tool...")
        get_root_req = {
            "jsonrpc": "2.0",
            "id": 5,
            "method": "tools/call",
            "params": {"name": "get_root", "arguments": {}}
        }
        req = urllib.request.Request(url, data=json.dumps(get_root_req).encode('utf-8'), headers={'Content-Type': 'application/json'})
        with urllib.request.urlopen(req) as response:
            res = json.loads(response.read().decode('utf-8'))
            text = res.get('result', {}).get('content', [])[0].get('text', '')
            if "Cymbal Superstore Inventory API" in text:
                 print("✓ tools/call (get_root) successful")
            else:
                 print(f"Error: Unexpected get_root response: {text}")
                 sys.exit(1)

        # 7. Call get_products
        print("Testing 'get_products' tool...")
        get_products_req = {
            "jsonrpc": "2.0",
            "id": 6,
            "method": "tools/call",
            "params": {"name": "get_products", "arguments": {}}
        }
        req = urllib.request.Request(url, data=json.dumps(get_products_req).encode('utf-8'), headers={'Content-Type': 'application/json'})
        with urllib.request.urlopen(req) as response:
            res = json.loads(response.read().decode('utf-8'))
            if "error" in res:
                 print(f"Error: get_products returned error: {res['error']}")
                 sys.exit(1)
            
            content = res.get('result', {}).get('content', [])
            if not content:
                 print(f"Error: get_products returned no content: {res}")
                 sys.exit(1)
            
            text = content[0].get('text', '')
            if text.startswith("[") and text.endswith("]"):
                 print(f"✓ tools/call (get_products) successful: Found {len(json.loads(text))} products")
            else:
                 print(f"Error: Unexpected get_products response: {text}")
                 sys.exit(1)

        print("\nAll tests passed!")

    except urllib.error.URLError as e:
        print(f"Connection failed: {e}")
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
        
        # Read remaining stderr (non-blocking if possible, but here we just read whatever is left)
        stderr_output = process.stderr.read()
        if stderr_output:
            print("Server Stderr:")
            print(stderr_output)

if __name__ == "__main__":
    run_test()