import subprocess
import json
import sys
import time
import requests
import threading
import queue
import os

def test_firestore():
    print("Testing Firestore tools over HTTP/SSE...")
    
    # Ensure project ID is set for the server
    env = os.environ.copy()
    if 'GOOGLE_CLOUD_PROJECT' not in env:
        # Try to get it from gcloud
        try:
            project = subprocess.check_output(['gcloud', 'config', 'get-value', 'project'], text=True).strip()
            if project:
                env['GOOGLE_CLOUD_PROJECT'] = project
                print(f"Using project: {project}")
        except:
            print("Warning: GOOGLE_CLOUD_PROJECT not set and gcloud failed.")

    server_process = None
    try:
        # Start the server process
        server_process = subprocess.Popen(
            ['./server'],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            env=env
        )
        
        # Give the server a moment to start
        time.sleep(3)
        
        # 1. Connect to SSE
        sse_url = "http://localhost:8080/sse"
        sse_response = requests.get(sse_url, stream=True, timeout=10)
        
        event_queue = queue.Queue()
        
        def sse_listener():
            try:
                event_type = None
                for line in sse_response.iter_lines():
                    if not line:
                        continue
                    line = line.decode('utf-8')
                    if line.startswith("event: "):
                        event_type = line[7:]
                    elif line.startswith("data: "):
                        data = line[6:]
                        event_queue.put({"event": event_type, "data": data})
                        event_type = None
            except Exception as e:
                pass

        listener_thread = threading.Thread(target=sse_listener, daemon=True)
        listener_thread.start()

        # Wait for the 'endpoint' event
        try:
            endpoint_event = event_queue.get(timeout=5)
            message_endpoint = endpoint_event['data']
            print(f"✓ Connected to SSE, message endpoint: {message_endpoint}")
        except queue.Empty:
            print("Timeout waiting for 'endpoint' event")
            return

        def send_message(msg):
            url = f"http://localhost:8080{message_endpoint}"
            resp = requests.post(url, json=msg, timeout=5)
            return resp.status_code == 202

        def wait_for_message(timeout=10):
            try:
                while True:
                    msg = event_queue.get(timeout=timeout)
                    if msg['event'] == 'message':
                        return json.loads(msg['data'])
            except queue.Empty:
                return None

        # Initialize
        send_message({
            "jsonrpc": "2.0", "id": 1, "method": "initialize",
            "params": {"protocolVersion": "2025-11-25", "capabilities": {}, "clientInfo": {"name": "test", "version": "1.0"}}
        })
        wait_for_message()
        send_message({"jsonrpc": "2.0", "method": "notifications/initialized"})

        # Test get_root
        print("Testing get_root...")
        send_message({"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "get_root", "arguments": {}}})
        res = wait_for_message()
        if res and 'result' in res:
            print(f"✓ get_root: {res['result']['content'][0]['text']}")
        else:
            print(f"✗ get_root failed: {res}")

        # Test get_products
        print("Testing get_products...")
        send_message({"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "get_products", "arguments": {}}})
        res = wait_for_message()
        if res and 'result' in res:
            content = res['result']['content'][0]['text']
            products = json.loads(content)
            print(f"✓ get_products: Found {len(products)} products")
        else:
            print(f"✗ get_products failed: {res}")

        # Test search
        print("Testing search...")
        send_message({"jsonrpc": "2.0", "id": 4, "method": "tools/call", "params": {"name": "search", "arguments": {"query": "apple"}}})
        res = wait_for_message()
        product_id = None
        if res and 'result' in res:
            content = res['result']['content'][0]['text']
            results = json.loads(content)
            print(f"✓ search (apple): Found {len(results)} products")
            if results:
                product_id = results[0]['id']
        else:
            print(f"✗ search failed: {res}")

        # Test get_product_by_id
        if product_id:
            print(f"Testing get_product_by_id ({product_id})...")
            send_message({"jsonrpc": "2.0", "id": 5, "method": "tools/call", "params": {"name": "get_product_by_id", "arguments": {"id": product_id}}})
            res = wait_for_message()
            if res and 'result' in res:
                content = res['result']['content'][0]['text']
                product = json.loads(content)
                print(f"✓ get_product_by_id: Found product {product['name']}")
            else:
                print(f"✗ get_product_by_id failed: {res}")

    except Exception as e:
        print(f"An error occurred: {e}")
    finally:
        if server_process:
            server_process.terminate()
            server_process.wait()

if __name__ == "__main__":
    test_firestore()