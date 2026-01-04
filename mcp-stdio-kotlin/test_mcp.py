import subprocess
import json
import sys
import threading

def read_stream(stream, callback):
    """Reads lines from a stream and calls callback."""
    for line in stream:
        callback(line)

def test_server():
    print("Starting server...")
    proc = subprocess.Popen(['./build/install/mcp-stdio-kotlin/bin/mcp-stdio-kotlin'], 
                            stdin=subprocess.PIPE, 
                            stdout=subprocess.PIPE, 
                            stderr=subprocess.PIPE,
                            text=True,
                            bufsize=1) # Line buffered

    # thread to print stderr to avoid blocking if buffer fills
    def print_stderr(line):
        print(f"STDERR: {line.strip()}", file=sys.stderr)
    
    stderr_thread = threading.Thread(target=read_stream, args=(proc.stderr, print_stderr), daemon=True)
    stderr_thread.start()

    def send_request(req):
        print(f"Sending request: {req['method']} (id={req.get('id')})")
        proc.stdin.write(json.dumps(req) + "\n")
        proc.stdin.flush()

    def read_response(expected_id):
        print(f"Waiting for response id={expected_id}...")
        while True:
            line = proc.stdout.readline()
            if not line:
                raise Exception("Server closed stdout unexpectedly")
            try:
                msg = json.loads(line)
                # Ignore notifications or other messages if not matching id (though strictly we expect response)
                if msg.get("id") == expected_id:
                    return msg
                else:
                    print(f"Received other message: {msg}")
            except json.JSONDecodeError:
                print(f"Received non-JSON: {line.strip()}")

    try:
        # 1. Initialize
        init_msg = {
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {
                "protocolVersion": "2024-11-05",
                "capabilities": {},
                "clientInfo": {"name": "test-client", "version": "1.0.0"}
            }
        }
        send_request(init_msg)
        resp1 = read_response(1)
        print(f"Initialize response: {json.dumps(resp1, indent=2)}")

        # 2. Call Tool (greet)
        greet_msg = {
            "jsonrpc": "2.0",
            "id": 2,
            "method": "tools/call",
            "params": {
                "name": "greet",
                "arguments": {"param": "World"}
            }
        }
        send_request(greet_msg)
        resp2 = read_response(2)
        print(f"Greet response: {json.dumps(resp2, indent=2)}")
        
        # Verify content
        content = resp2.get("result", {}).get("content", [])
        if content and content[0].get("text") == "Hello, World!":
            print("SUCCESS: Greeting is correct.")
        else:
            print("FAILURE: Greeting is incorrect.")
            sys.exit(1)

        # 3. Call Tool (missing param) - verify robustness/default
        greet_msg_missing = {
            "jsonrpc": "2.0",
            "id": 3,
            "method": "tools/call",
            "params": {
                "name": "greet",
                "arguments": {} 
            }
        }
        send_request(greet_msg_missing)
        resp3 = read_response(3)
        print(f"Greet (missing param) response: {json.dumps(resp3, indent=2)}")

        content = resp3.get("result", {}).get("content", [])
        if content and content[0].get("text") == "Hello, World!":
             print("SUCCESS: Default greeting is correct.")
        else:
             print("FAILURE: Default greeting is incorrect.")
             sys.exit(1)

    finally:
        print("Terminating server...")
        proc.terminate()
        proc.wait()

if __name__ == "__main__":
    test_server()