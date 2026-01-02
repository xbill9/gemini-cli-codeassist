import subprocess
import json
import time
import os

def test_server():
    print("Starting server...")
    proc = subprocess.Popen(['./build/install/mcp-stdio-kotlin/bin/mcp-stdio-kotlin'], 
                            stdin=subprocess.PIPE, 
                            stdout=subprocess.PIPE, 
                            stderr=subprocess.PIPE,
                            text=True,
                            bufsize=1)

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

    print("Sending initialize request...")
    proc.stdin.write(json.dumps(init_msg) + "\n")
    proc.stdin.flush()

    time.sleep(1)
    
    greet_msg = {
        "jsonrpc": "2.0",
        "id": 2,
        "method": "tools/call",
        "params": {
            "name": "greet",
            "arguments": {"param": "World"}
        }
    }
    print("Sending greet request...")
    proc.stdin.write(json.dumps(greet_msg) + "\n")
    proc.stdin.flush()

    print("Waiting for response...")
    time.sleep(2)
    try:
        # Set stdout to non-blocking
        import fcntl
        fd = proc.stdout.fileno()
        fl = fcntl.fcntl(fd, fcntl.F_GETFL)
        fcntl.fcntl(fd, fcntl.F_SETFL, fl | os.O_NONBLOCK)
        
        output = proc.stdout.read()
        if output:
            print(f"STDOUT: {output}")
        else:
            print("STDOUT is empty")
    except Exception as e:
        print(f"Error reading stdout: {e}")

    # Give it a moment to print to stderr
    time.sleep(1)
    
    # Read remaining stderr
    try:
        # We need to set stderr to non-blocking or just read what's available
        import fcntl
        fd = proc.stderr.fileno()
        fl = fcntl.fcntl(fd, fcntl.F_GETFL)
        fcntl.fcntl(fd, fcntl.F_SETFL, fl | os.O_NONBLOCK)
        err = proc.stderr.read()
        if err:
            print(f"STDERR: {err}")
    except Exception as e:
        print(f"Error reading stderr: {e}")

    proc.terminate()

if __name__ == "__main__":
    test_server()
