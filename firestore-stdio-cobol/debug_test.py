import json
import subprocess
import sys
import threading

def read_stderr(process):
    for line in iter(process.stderr.readline, ''):
        print(f"STDERR: {line.strip()}")

def test():
    process = subprocess.Popen(
        ['./firestore-server'],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        bufsize=1
    )

    stderr_thread = threading.Thread(target=read_stderr, args=(process,))
    stderr_thread.daemon = True
    stderr_thread.start()

    def send(msg):
        print(f"SEND: {json.dumps(msg)}")
        process.stdin.write(json.dumps(msg) + "\n")
        process.stdin.flush()

    def receive():
        print("WAITING FOR RECEIVE...")
        line = process.stdout.readline()
        if not line:
            print("RECEIVE: EOF")
            return None
        print(f"RECEIVE: {line.strip()}")
        return json.loads(line)

    try:
        send({
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {
                "protocolVersion": "2024-11-05",
                "capabilities": {},
                "clientInfo": {"name": "debug-client", "version": "1.0.0"}
            }
        })
        receive()

        send({"jsonrpc": "2.0", "method": "notifications/initialized", "params": {}})
        
        send({"jsonrpc": "2.0", "id": 2, "method": "tools/list", "params": {}})
        receive()

    except Exception as e:
        print(f"Exception: {e}")
    finally:
        process.terminate()

if __name__ == "__main__":
    test()