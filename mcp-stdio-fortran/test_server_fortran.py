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
            ['./server-fortran'],
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
        if 'greet' not in tools:
            print(f"Error: 'greet' tool not found in list. Found: {tools}")
            sys.exit(1)
        print("✓ tools/list successful")

        # Step 4: Test tools/call (greet)
        send(call_req)
        res = receive()
        if not res:
            print("Error: No response from server for tools/call")
            sys.exit(1)
            
        content = res.get('result', {}).get('content', [])
        text = content[0].get('text') if content else ""
        
        if text != "Hello, Gemini!":
            print(f"Error: Unexpected response text: {text}")
            sys.exit(1)
        print("✓ tools/call (greet) successful")

        # Cleanup
        process.terminate()
        print("\nAll tests passed!")

    except Exception as e:
        print(f"An error occurred: {e}")
        sys.exit(1)

if __name__ == "__main__":
    test_greet()
