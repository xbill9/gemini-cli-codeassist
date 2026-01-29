import json
import subprocess
import sys

def test_firestore_tools():
    print("Testing Firestore COBOL Server...")
    process = subprocess.Popen(
        ['./firestore-server'],
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

    # 1. Initialize
    init_request = {
        "jsonrpc": "2.0",
        "id": 1,
        "method": "initialize",
        "params": {
            "protocolVersion": "2024-11-05",
            "capabilities": {},
            "clientInfo": {"name": "test-client", "version": "1.0.0"}
        }
    }
    send(init_request)
    res = receive()
    if not res or "result" not in res:
        print(f"Error during initialize: {res}")
        sys.exit(1)
    print("✓ initialize successful")

    # 2. Initialized notification
    send({"jsonrpc": "2.0", "method": "notifications/initialized"})
    print("✓ notifications/initialized sent")

    # 3. List tools
    send({"jsonrpc": "2.0", "id": 2, "method": "tools/list", "params": {}})
    res = receive()
    tools = [t['name'] for t in res.get('result', {}).get('tools', [])]
    required_tools = ['greet', 'check_db', 'get_root', 'get_product_by_id', 'seed', 'reset']
    missing_tools = [t for t in required_tools if t not in tools]
    if missing_tools:
        print(f"Error: Missing tools: {missing_tools}. Found: {tools}")
        sys.exit(1)
    print("✓ tools/list successful")

    # 4. Test greet
    send({
        "jsonrpc": "2.0", "id": 3, "method": "tools/call",
        "params": {"name": "greet", "arguments": {"param": "COBOL"}}
    })
    res = receive()
    text = res.get('result', {}).get('content', [])[0].get('text', '')
    if text != "Hello, COBOL!":
        print(f"Error: Greet failed: {text}")
        sys.exit(1)
    print("✓ greet successful")

    # 5. Test check_db
    send({"jsonrpc": "2.0", "id": 4, "method": "tools/call", "params": {"name": "check_db", "arguments": {}}})
    res = receive()
    text = res.get('result', {}).get('content', [])[0].get('text', '')
    if "Database running:" not in text:
        print(f"Error: check_db failed: {text}")
        sys.exit(1)
    print(f"✓ check_db successful: {text}")

    # 6. Test get_root
    send({"jsonrpc": "2.0", "id": 5, "method": "tools/call", "params": {"name": "get_root", "arguments": {}}})
    res = receive()
    text = res.get('result', {}).get('content', [])[0].get('text', '')
    if "Cymbal Superstore Inventory API" not in text:
        print(f"Error: get_root failed: {text}")
        sys.exit(1)
    print("✓ get_root successful")

    # 7. Test get_products (might be empty if not seeded)
    send({"jsonrpc": "2.0", "id": 6, "method": "tools/call", "params": {"name": "get_products", "arguments": {}}})
    res = receive()
    text = res.get('result', {}).get('content', [])[0].get('text', '')
    print(f"✓ get_products successful (got {len(text)} bytes)")

    # 8. Test seed
    print("Seeding database...")
    send({"jsonrpc": "2.0", "id": 7, "method": "tools/call", "params": {"name": "seed", "arguments": {}}})
    res = receive()
    text = res.get('result', {}).get('content', [])[0].get('text', '')
    if "seeded" not in text:
        print(f"Error: seed failed: {text}")
        sys.exit(1)
    print("✓ seed successful")

    # Cleanup
    process.terminate()
    print("\nTests passed!")

if __name__ == "__main__":
    test_firestore_tools()