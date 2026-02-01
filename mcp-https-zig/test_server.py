import subprocess
import json
import sys
import socket
import time
import os

def test_server():
    print("Testing server tools (HTTP)...")
    
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
    
    process = None
    
    try:
        # Start server
        process = subprocess.Popen(
            ['./server'],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            bufsize=1 # Line buffered
        )
        
        # Give it a moment to bind to port
        time.sleep(2)

        def send_and_receive(msg):
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.settimeout(5)
            try:
                sock.connect(('127.0.0.1', 8080))
                body = json.dumps(msg)
                req = (
                    f"POST / HTTP/1.1\r\n"
                    f"Host: 127.0.0.1:8080\r\n"
                    f"Content-Type: application/json\r\n"
                    f"Content-Length: {len(body)}\r\n"
                    f"Connection: close\r\n"
                    f"\r\n"
                    f"{body}"
                )
                sock.sendall(req.encode('utf-8'))

                # Minimalistic HTTP receiver
                buffer = b""
                while True:
                    try:
                        chunk = sock.recv(4096)
                    except socket.timeout:
                        break
                    if not chunk:
                        break
                    buffer += chunk
                    
                    # Find end of headers
                    idx = buffer.find(b"\r\n\r\n")
                    if idx != -1:
                        # Parse Content-Length
                        headers = buffer[:idx].decode('utf-8')
                        cl = -1
                        for line in headers.split('\r\n'):
                            if line.lower().startswith("content-length:"):
                                cl = int(line.split(':')[1].strip())
                        
                        if cl == 0:
                             return {} # 202 Accepted case
                        
                        body_start = idx + 4
                        if cl != -1 and len(buffer) - body_start >= cl:
                            # We have the full body
                            body = buffer[body_start:body_start+cl]
                            try:
                                return json.loads(body.decode('utf-8'))
                            except json.JSONDecodeError:
                                return None
                return None
            finally:
                sock.close()

        # 1. Initialize
        res = send_and_receive(init_req)
        if not res or "result" not in res:
            raise Exception(f"Initialize failed: {res}")
        print("✓ initialize")

        send_and_receive(initialized_notif)
        print("✓ notifications/initialized")

        # 2. List Tools
        res = send_and_receive(list_req)
        if not res or "result" not in res:
            raise Exception(f"List tools failed: {res}")
        
        tools = [t['name'] for t in res.get('result', {}).get('tools', [])]
        expected_tools = ['greet']
        for t in expected_tools:
            if t not in tools:
                raise Exception(f"Tool {t} not found in {tools}")
        print("✓ tools/list")

        # 3. Call greet
        res = send_and_receive(call_greet)
        if not res or "result" not in res:
            raise Exception(f"Call greet failed: {res}")
        
        content = res['result'].get('content', [])
        if not content:
             raise Exception(f"Greet failed: empty content")
        
        # Structure might be content[0]['text'] or content[0]['text']['text']
        item = content[0]
        text = ""
        if 'text' in item:
            if isinstance(item['text'], dict):
                text = item['text'].get('text', '')
            else:
                text = item['text']

        if text != "Gemini":
            raise Exception(f"Greet failed: expected 'Gemini', got '{text}' (Full res: {res})")
        print("✓ tools/call (greet)")

        print("\nAll tests passed!")

    except Exception as e:
        print(f"\nError: {e}")
        if process:
            # Try to read stderr
            print("\nServer stderr:")
            # Set to non-blocking to read what's available
            os.set_blocking(process.stderr.fileno(), False)
            print(process.stderr.read() or "(empty)")
        sys.exit(1)
    finally:
        if process:
            process.terminate()
            try:
                process.wait(timeout=1)
            except subprocess.TimeoutExpired:
                process.kill()

if __name__ == "__main__":
    test_server()
