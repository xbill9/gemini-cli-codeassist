import subprocess
import json
import time
import os
import sys

def read_json_rpc(proc):
    """Reads a single JSON-RPC message from stdout."""
    while True:
        line = proc.stdout.readline()
        if not line:
            return None
        line = line.strip()
        if not line:
            continue
        try:
            return json.loads(line)
        except json.JSONDecodeError:
            print(f"Server output (not JSON): {line}", file=sys.stderr)

def test_server():
    print("Building server...")
    subprocess.run(["./gradlew", "installDist"], check=True, capture_output=True)

    print("Starting server...")
    # Path to the generated start script
    script_path = './build/install/firestore-stdio-kotlin/bin/firestore-stdio-kotlin'
    
    if not os.path.exists(script_path):
        print(f"Error: Start script not found at {script_path}")
        return

    proc = subprocess.Popen([script_path], 
                            stdin=subprocess.PIPE, 
                            stdout=subprocess.PIPE, 
                            stderr=sys.stderr, # Pipe stderr directly to console
                            text=True,
                            bufsize=1)

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
        print("-> Sending initialize request...")
        proc.stdin.write(json.dumps(init_msg) + "\n")
        proc.stdin.flush()

        response = read_json_rpc(proc)
        print(f"<- Received initialize response: {json.dumps(response, indent=2)}")

        # 2. Initialized Notification
        initialized_msg = {
            "jsonrpc": "2.0",
            "method": "notifications/initialized",
            "params": {}
        }
        print("-> Sending initialized notification...")
        proc.stdin.write(json.dumps(initialized_msg) + "\n")
        proc.stdin.flush()

        # 3. Call Tool: check_db
        call_msg = {
            "jsonrpc": "2.0",
            "id": 2,
            "method": "tools/call",
            "params": {
                "name": "check_db",
                "arguments": {}
            }
        }
        print("-> Sending tool/call (check_db)...")
        proc.stdin.write(json.dumps(call_msg) + "\n")
        proc.stdin.flush()

        response = read_json_rpc(proc)
        print(f"<- Received tool response: {json.dumps(response, indent=2)}")

    finally:
        print("Terminating server...")
        proc.terminate()
        try:
            proc.wait(timeout=2)
        except subprocess.TimeoutExpired:
            proc.kill()

if __name__ == "__main__":
    test_server()