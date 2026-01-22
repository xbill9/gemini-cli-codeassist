import subprocess
import json
import sys

def repro():
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
    
    call_req = {
        "jsonrpc": "2.0",
        "id": 2,
        "method": "tools/call",
        "params": {
            "name": "mcpc-info",
            "arguments": {}
        }
    }

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

    print("Sending initialize...")
    send(init_req)
    print(f"Received: {receive()}")

    print("Sending tools/call (mcpc-info)...")
    send(call_req)
    res = receive()
    print(f"Received: {res}")
    
    stderr = process.stderr.read()
    if stderr:
        print(f"Stderr:\n{stderr}")

    process.terminate()

if __name__ == "__main__":
    repro()
