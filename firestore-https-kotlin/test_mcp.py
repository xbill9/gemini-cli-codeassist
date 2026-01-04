import requests
import threading
import time
import json
import uuid
import sys
from urllib.parse import urlparse, parse_qs

BASE_URL = "http://localhost:8080"
SERVER_SESSION_ID = None
INIT_EVENT = threading.Event()

def listen_sse():
    global SERVER_SESSION_ID
    print(f"Connecting to SSE...", file=sys.stderr)
    url = f"{BASE_URL}/sse"
    try:
        response = requests.get(url, stream=True)
        if response.status_code != 200:
             print(f"Failed to connect to SSE: {response.status_code}", file=sys.stderr)
             return

        for line in response.iter_lines():
            if line:
                decoded_line = line.decode('utf-8')
                print(f"SSE Raw: {decoded_line}", file=sys.stderr)
                
                if decoded_line.startswith("event: endpoint"):
                    # The next line should be data: ...
                    pass
                elif decoded_line.startswith("data: "):
                    data = decoded_line[6:]
                    # Check if this looks like the endpoint URL
                    if "/messages?sessionId=" in data:
                        # Extract session ID
                        try:
                            parsed = urlparse(data)
                            qs = parse_qs(parsed.query)
                            SERVER_SESSION_ID = qs['sessionId'][0]
                            print(f"Captured Session ID: {SERVER_SESSION_ID}", file=sys.stderr)
                            INIT_EVENT.set()
                        except Exception as e:
                            print(f"Error parsing session ID: {e}", file=sys.stderr)
                    else:
                        # It's a normal message
                        try:
                            json_data = json.loads(data)
                            print(f"Received JSON: {json.dumps(json_data, indent=2)}")
                        except:
                            print(f"Received Data: {data}")

    except Exception as e:
        print(f"SSE Error: {e}", file=sys.stderr)

def send_rpc():
    print("Waiting for session ID...", file=sys.stderr)
    if not INIT_EVENT.wait(timeout=10):
        print("Timed out waiting for session ID", file=sys.stderr)
        return

    print(f"Using Session ID: {SERVER_SESSION_ID}", file=sys.stderr)
    
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
    
    post_url = f"{BASE_URL}/messages?sessionId={SERVER_SESSION_ID}"
    
    try:
        # Force pretty-printed JSON to test server handling of newlines
        res = requests.post(post_url, data=json.dumps(init_msg, indent=2), headers={"Content-Type": "application/json"})
        print(f"Initialize POST status: {res.status_code}", file=sys.stderr)
    except Exception as e:
        print(f"POST Error: {e}", file=sys.stderr)

    time.sleep(1)

    print("Sending tools/list request...", file=sys.stderr)
    list_msg = {
        "jsonrpc": "2.0",
        "id": 2,
        "method": "tools/list",
        "params": {}
    }
    try:
        res = requests.post(post_url, json=list_msg)
        print(f"List POST status: {res.status_code}", file=sys.stderr)
    except Exception as e:
        print(f"POST Error: {e}", file=sys.stderr)

    time.sleep(1)

    print("Sending greet request...", file=sys.stderr)
    greet_msg = {
        "jsonrpc": "2.0",
        "id": 3,
        "method": "tools/call",
        "params": {
            "name": "greet",
            "arguments": {"name": "Galaxy"}
        }
    }
    try:
        res = requests.post(post_url, json=greet_msg)
        print(f"Greet POST status: {res.status_code}", file=sys.stderr)
    except Exception as e:
        print(f"POST Error: {e}", file=sys.stderr)

if __name__ == "__main__":
    t = threading.Thread(target=listen_sse, daemon=True)
    t.start()
    
    send_rpc()
    time.sleep(5)
