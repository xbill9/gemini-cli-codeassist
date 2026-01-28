import subprocess
import json
import sys

def test_greet():
    print("Testing 'greet' tool...")
    
    # 1. Initialize
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
    
    # 2. Initialized notification
    initialized_notif = {
        "jsonrpc": "2.0",
        "method": "notifications/initialized"
    }
    
    # 3. List tools
    list_req = {
        "jsonrpc": "2.0",
        "id": 2,
        "method": "tools/list",
        "params": {}
    }
    
    # 4. Call greet
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

    try:
        # Start the server process
        process = subprocess.Popen(
            ['./firestore-server'],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True
        )

        def send(msg):
            process.stdin.write(json.dumps(msg) + "\n")
            process.stdin.flush()

        def receive():
            line = process.stdout.readline()
            if not line:
                return None
            return json.loads(line)

        # Step 1: Initialize
        send(init_req)
        res = receive()
        if not res or "result" not in res:
            print(f"Error during initialize: {res}")
            sys.exit(1)
        print("✓ initialize successful")

        # Step 2: Initialized notification
        send(initialized_notif)
        print("✓ notifications/initialized sent")

        # Step 3: Test tools/list
        send(list_req)
        res = receive()
        if not res:
            print("Error: No response from server for tools/list")
            sys.exit(1)
        
        tools = [t['name'] for t in res.get('result', {}).get('tools', [])]
        required_tools = ['greet', 'check_db', 'get_root', 'get_products', 'get_product_by_id', 'seed', 'reset']
        missing_tools = [t for t in required_tools if t not in tools]
        
        if missing_tools:
            print(f"Error: Missing tools: {missing_tools}. Found: {tools}")
            sys.exit(1)
        print("✓ tools/list successful (all tools found)")

        # Step 4: Test tools/call (greet)
        send(call_req)
        res = receive()
        if not res:
            print("Error: No response from server for tools/call (greet)")
            sys.exit(1)
            
        content = res.get('result', {}).get('content', [])
        text = content[0].get('text') if content else ""
        
        if text != "Hello, Gemini!":
            print(f"Error: Unexpected response text for greet: {text}")
            sys.exit(1)
        print("✓ tools/call (greet) successful")

        # Step 5: Test tools/call (get_root)
        print("Testing 'get_root' tool...")
        get_root_req = {
            "jsonrpc": "2.0",
            "id": 4,
            "method": "tools/call",
            "params": {"name": "get_root", "arguments": {}}
        }
        send(get_root_req)
        res = receive()
        if not res:
            print("Error: No response for get_root")
            sys.exit(1)
        
        text = res.get('result', {}).get('content', [])[0].get('text', '')
        if "Cymbal Superstore Inventory API" not in text:
             print(f"Error: Unexpected get_root response: {text}")
             sys.exit(1)
        print("✓ tools/call (get_root) successful")

        # Step 6: Test tools/call (check_db)
        print("Testing 'check_db' tool...")
        check_db_req = {
            "jsonrpc": "2.0",
            "id": 5,
            "method": "tools/call",
            "params": {"name": "check_db", "arguments": {}}
        }
        send(check_db_req)
        res = receive()
        if not res:
            print("Error: No response for check_db")
            sys.exit(1)
        
        text = res.get('result', {}).get('content', [])[0].get('text', '')
        if "Database running:" not in text:
             print(f"Error: Unexpected check_db response: {text}")
             sys.exit(1)
        print(f"✓ tools/call (check_db) successful: {text}")

        # Cleanup
        process.terminate()
        print("\nAll tests passed!")

    except Exception as e:
        print(f"An error occurred: {e}")
        sys.exit(1)

if __name__ == "__main__":
    test_greet()
