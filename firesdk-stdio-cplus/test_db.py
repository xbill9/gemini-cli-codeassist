import subprocess
import json
import sys
import time

def send(process, msg):
    process.stdin.write(json.dumps(msg) + "\n")
    process.stdin.flush()

def receive(process):
    line = process.stdout.readline()
    if not line:
        return None
    try:
        return json.loads(line)
    except:
        print(f"Failed to parse JSON: {line}")
        return None

def main():
    process = subprocess.Popen(
        ['./server'],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True
    )

    # 1. Initialize
    send(process, {
        "jsonrpc": "2.0",
        "id": 1,
        "method": "initialize",
        "params": {
            "protocolVersion": "2024-11-05",
            "capabilities": {},
            "clientInfo": {"name": "test-client", "version": "1.0"}
        }
    })
    receive(process)

    send(process, {
        "jsonrpc": "2.0",
        "method": "notifications/initialized"
    })

    # 2. Check DB
    send(process, {
        "jsonrpc": "2.0",
        "id": 2,
        "method": "tools/call",
        "params": {"name": "check_db", "arguments": {}}
    })
    res = receive(process)
    print(f"Check DB: {res['result']['content'][0]['text']}")

    # 3. Seed DB (this might take a while as it calls REST multiple times)
    print("Seeding DB...")
    send(process, {
        "jsonrpc": "2.0",
        "id": 3,
        "method": "tools/call",
        "params": {"name": "seed", "arguments": {}}
    })
    res = receive(process)
    print(f"Seed DB: {res['result']['content'][0]['text']}")

    # 4. Get Products
    print("Getting products...")
    send(process, {
        "jsonrpc": "2.0",
        "id": 4,
        "method": "tools/call",
        "params": {"name": "get_products", "arguments": {}}
    })
    res = receive(process)
    products_json = res['result']['content'][0]['text']
    products = json.loads(products_json)
    print(f"Found {len(products)} products")
    if len(products) > 0:
        print(f"First product: {products[0]['name']}")

    process.terminate()

if __name__ == "__main__":
    main()
