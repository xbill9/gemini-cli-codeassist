import subprocess
import json
import sys

def test_server():
    print("Testing server tools...")
    
    # Requests
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
    
    initialized_notif = {
        "jsonrpc": "2.0",
        "method": "notifications/initialized"
    }
    
    list_req = {
        "jsonrpc": "2.0",
        "id": 2,
        "method": "tools/list",
        "params": {}
    }

    # Tool calls
    call_greet = {
        "jsonrpc": "2.0", "id": 3, "method": "tools/call",
        "params": { "name": "greet", "arguments": { "param": "Gemini" } }
    }
    
    call_sys = {
        "jsonrpc": "2.0", "id": 4, "method": "tools/call",
        "params": { "name": "get_system_info", "arguments": {} }
    }
    
    call_srv = {
        "jsonrpc": "2.0", "id": 5, "method": "tools/call",
        "params": { "name": "get_server_info", "arguments": {} }
    }

    call_time = {
        "jsonrpc": "2.0", "id": 6, "method": "tools/call",
        "params": { "name": "get_current_time", "arguments": {} }
    }

    try:
        process = subprocess.Popen(
            ['./server'],
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

        # 1. Initialize
        send(init_req)
        res = receive()
        if not res or "result" not in res:
            raise Exception(f"Initialize failed: {res}")
        print("✓ initialize")

        send(initialized_notif)

        # 2. List Tools
        send(list_req)
        res = receive()
        tools = [t['name'] for t in res.get('result', {}).get('tools', [])]
        expected_tools = ['greet', 'get_system_info', 'get_server_info', 'get_current_time']
        for t in expected_tools:
            if t not in tools:
                raise Exception(f"Tool {t} not found in {tools}")
        print("✓ tools/list")

        # 3. Call greet
        send(call_greet)
        res = receive()
        text = res['result']['content'][0]['text']
        if text != "Gemini":
            raise Exception(f"Greet failed: {text}")
        print("✓ tools/call (greet)")

        # 4. Call get_system_info
        send(call_sys)
        res = receive()
        text = res['result']['content'][0]['text']
        if "System Name:" not in text:
             raise Exception(f"System info failed: {text}")
        print("✓ tools/call (get_system_info)")

        # 5. Call get_server_info
        send(call_srv)
        res = receive()
        text = res['result']['content'][0]['text']
        if "firestore-stdio-c" not in text:
             raise Exception(f"Server info failed: {text}")
        print("✓ tools/call (get_server_info)")

        # 6. Call get_current_time
        send(call_time)
        res = receive()
        text = res['result']['content'][0]['text']
        # Simple check for date format or length
        if len(text) < 10:
             raise Exception(f"Time failed: {text}")
        print("✓ tools/call (get_current_time)")

        process.terminate()
        print("\nAll tests passed!")

    except Exception as e:
        print(f"\nError: {e}")
        # Ensure we don't leave zombie process if possible
        if 'process' in locals():
            process.terminate()
        sys.exit(1)

if __name__ == "__main__":
    test_server()
