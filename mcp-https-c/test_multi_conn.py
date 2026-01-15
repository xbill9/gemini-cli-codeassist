import subprocess
import json
import sys
import socket
import time

def test_multi_conn():
    print("Testing server tools with multiple connections...")
    
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

    process = None

    try:
        # Start server
        log_file = open("server_test.log", "w")
        process = subprocess.Popen(
            ['./server'],
            stdout=subprocess.PIPE,
            stderr=log_file,
            text=True
        )
        
        # Give it a moment to bind to port
        time.sleep(1)

        def send_and_receive(msg, existing_sock=None):
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
            
            if existing_sock:
                sock = existing_sock
            else:
                sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
                sock.connect(('127.0.0.1', 8080))
            
            sock.sendall(req.encode('utf-8'))

            # Minimalistic HTTP receiver
            buffer = b""
            while True:
                chunk = sock.recv(4096)
                if not chunk:
                    break
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
                        if cl == 0:
                            if not existing_sock:
                                sock.close()
                            return None
                        body = buffer[body_start:body_start+cl]
                        if not existing_sock:
                            sock.close()
                        try:
                            return json.loads(body.decode('utf-8'))
                        except json.JSONDecodeError:
                            return None
            if not existing_sock:
                sock.close()
            return None

        # 1. Initialize on first connection
        print("Initializing on Connection 1...")
        res = send_and_receive(init_req)
        if not res or "result" not in res:
            raise Exception(f"Initialize failed: {res}")
        print("✓ initialize")

        # 2. Initialized notification (optional but good practice)
        send_and_receive(initialized_notif)
        print("✓ notifications/initialized")

        # 3. Initialize and List on SECOND connection (using persistent connection)
        print("Initializing on Connection 2...")
        sock2 = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock2.connect(('127.0.0.1', 8080))
        
        res = send_and_receive(init_req, existing_sock=sock2)
        if not res or "result" not in res:
            raise Exception(f"Initialize failed on Connection 2: {res}")
        print("✓ initialize (on Connection 2)")

        send_and_receive(initialized_notif, existing_sock=sock2)
        print("✓ notifications/initialized (on Connection 2)")

        print("Listing tools on Connection 2...")
        res = send_and_receive(list_req, existing_sock=sock2)
        if not res or "result" not in res:
            raise Exception(f"List tools failed on Connection 2: {res}")
        
        print("✓ tools/list (on Connection 2)")
        sock2.close()
        print("\nMulti-connection test passed!")

    except Exception as e:
        print(f"\nError: {e}")
        if process:
            # Wait a bit for logs to flush
            time.sleep(0.5)
            stderr = process.stderr.read()
            print("\nServer Stderr:")
            print(stderr)
        sys.exit(1)
    finally:
        if process:
            process.terminate()

if __name__ == "__main__":
    test_multi_conn()
