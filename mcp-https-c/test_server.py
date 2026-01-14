import subprocess
import json
import sys
import socket
import time

def test_server():
    print("Testing server tools (TCP)...")
    
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
    
    call_info = {
        "jsonrpc": "2.0", "id": 7, "method": "tools/call",
        "params": { "name": "mcpc-info", "arguments": {} }
    }

    process = None
    sock = None

    try:
        # Start server
        process = subprocess.Popen(
            ['./server'],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True
        )
        
        # Give it a moment to bind to port
        time.sleep(1)

        # Connect via TCP
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.connect(('127.0.0.1', 8080))

        def send(msg):
            body = json.dumps(msg)
            req = (
                f"POST / HTTP/1.1\r\n"
                f"Host: 127.0.0.1:8080\r\n"
                f"Content-Type: application/json\r\n"
                f"Content-Length: {len(body)}\r\n"
                f"\r\n"
                f"{body}"
            )
            sock.sendall(req.encode('utf-8'))

        def receive():
            # Minimalistic HTTP receiver
            buffer = b""
            while True:
                chunk = sock.recv(4096)
                if not chunk:
                    return None
                buffer += chunk
                
                # Find end of headers
                idx = buffer.find(b"\r\n\r\n")
                if idx != -1:
                    # Parse Content-Length
                    headers = buffer[:idx].decode('utf-8')
                    cl = 0
                    for line in headers.split('\r\n'):
                        if line.lower().startswith("content-length:"):
                            cl = int(line.split(':')[1].strip())
                    
                    body_start = idx + 4
                    if len(buffer) - body_start >= cl:
                        # We have the full body
                        body = buffer[body_start:body_start+cl]
                        try:
                            return json.loads(body.decode('utf-8'))
                        except json.JSONDecodeError:
                            return None
            return None

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
        expected_tools = ['greet', 'get_system_info', 'get_server_info', 'get_current_time', 'mcpc-info']
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
        if "mcp-https-c" not in text:
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

        # 7. Call mcpc-info
        send(call_info)
        res = receive()
        text = res['result']['content'][0]['text']
        if "mcpc library" not in text:
             raise Exception(f"Info failed: {text}")
        print("✓ tools/call (mcpc-info)")

        print("\nAll tests passed!")

    except Exception as e:
        print(f"\nError: {e}")
        sys.exit(1)
    finally:
        if sock:
            sock.close()
        if process:
            process.terminate()
            try:
                process.wait(timeout=1)
            except subprocess.TimeoutExpired:
                process.kill()

if __name__ == "__main__":
    test_server()
