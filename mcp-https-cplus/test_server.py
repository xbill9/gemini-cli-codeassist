import subprocess
import json
import sys
import time
import requests
import threading
import queue

def test_greet():
    print("Testing 'greet' tool over HTTP/SSE...")
    
    server_process = None
    try:
        # Start the server process
        server_process = subprocess.Popen(
            ['./server'],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True
        )
        
        # Give the server a moment to start
        time.sleep(2)
        
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
                print(f"SSE listener error: {e}")

        listener_thread = threading.Thread(target=sse_listener, daemon=True)
        listener_thread.start()

        # Wait for the 'endpoint' event
        try:
            endpoint_event = event_queue.get(timeout=5)
            if endpoint_event['event'] != 'endpoint':
                print(f"Expected 'endpoint' event, got: {endpoint_event}")
                sys.exit(1)
            
            message_endpoint = endpoint_event['data']
            print(f"✓ Connected to SSE, message endpoint: {message_endpoint}")
        except queue.Empty:
            print("Timeout waiting for 'endpoint' event")
            sys.exit(1)

        def send_message(msg):
            url = f"http://localhost:8080{message_endpoint}"
            resp = requests.post(url, json=msg, timeout=5)
            if resp.status_code != 202:
                print(f"Error sending message: {resp.status_code} {resp.text}")
                return False
            return True

        def wait_for_message(timeout=5):
            try:
                while True:
                    msg = event_queue.get(timeout=timeout)
                    if msg['event'] == 'message':
                        return json.loads(msg['data'])
                    # Ignore heartbeats or other events
            except queue.Empty:
                return None

        init_req = {
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {
                "protocolVersion": "2025-11-25",
                "capabilities": {},
                "clientInfo": {"name": "test-client", "version": "1.0"}
            }
        }
        
        if not send_message(init_req):
            sys.exit(1)
            
        res = wait_for_message()
        if not res or "result" not in res:
            print(f"Error during initialize: {res}")
            sys.exit(1)
        print("✓ initialize successful")

        # 3. Initialized notification
        initialized_notif = {
            "jsonrpc": "2.0",
            "method": "notifications/initialized"
        }
        send_message(initialized_notif)
        print("✓ notifications/initialized sent")

        # 4. List tools
        list_req = {
            "jsonrpc": "2.0",
            "id": 2,
            "method": "tools/list",
            "params": {}
        }
        send_message(list_req)
        res = wait_for_message()
        if not res:
            print("Error: No response from server for tools/list")
            sys.exit(1)
        
        tools = [t['name'] for t in res.get('result', {}).get('tools', [])]
        if 'greet' not in tools:
            print(f"Error: 'greet' tool not found in list. Found: {tools}")
            sys.exit(1)
        print("✓ tools/list successful")

        # 5. Call greet
        call_req = {
            "jsonrpc": "2.0",
            "id": 3,
            "method": "tools/call",
            "params": {
                "name": "greet",
                "arguments": {
                    "param": "Gemini"
                }
            }
        }
        send_message(call_req)
        res = wait_for_message()
        if not res:
            print("Error: No response from server for tools/call")
            sys.exit(1)
            
        content = res.get('result', {}).get('content', [])
        text = content[0].get('text') if content else ""
        
        if text != "Gemini":
            print(f"Error: Unexpected response text: {text}")
            sys.exit(1)
        print("✓ tools/call (greet) successful")

        print("\nAll tests passed!")

    except Exception as e:
        print(f"An error occurred: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)
    finally:
        if server_process:
            server_process.terminate()
            server_process.wait()

if __name__ == "__main__":
    test_greet()