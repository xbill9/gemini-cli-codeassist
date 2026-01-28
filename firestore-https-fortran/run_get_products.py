import json
import urllib.request
import time
import subprocess
import os

def call_get_products():
    port = "8081"
    env = os.environ.copy()
    env["PORT"] = port
    
    process = subprocess.Popen(
        ['./firestore-https-server'],
        env=env,
        stderr=subprocess.PIPE,
        text=True
    )
    time.sleep(2)
    
    url = f"http://localhost:{port}/mcp"
    
    try:
        # Initialize
        init_req = {
            "jsonrpc": "2.0", "id": 1, "method": "initialize",
            "params": {"protocolVersion": "2024-11-05", "capabilities": {}, "clientInfo": {"name": "test", "version": "1.0"}}
        }
        urllib.request.urlopen(urllib.request.Request(url, data=json.dumps(init_req).encode('utf-8'), headers={'Content-Type': 'application/json'}))
        
        # Call get_products
        call_req = {
            "jsonrpc": "2.0", "id": 2, "method": "tools/call",
            "params": {"name": "get_products", "arguments": {}}
        }
        req = urllib.request.Request(url, data=json.dumps(call_req).encode('utf-8'), headers={'Content-Type': 'application/json'})
        with urllib.request.urlopen(req) as response:
            res = json.loads(response.read().decode('utf-8'))
            print(json.dumps(res, indent=2))
            
    finally:
        process.terminate()
        print("Server logs:")
        print(process.stderr.read())

if __name__ == "__main__":
    call_get_products()
