import 'package:firestore_https_flutter/firestore_https_flutter.dart';

void main() async {
  // This is a simple example of how to use the server programmatically.
  final server = createServer();
  print('Server created: $server');

  // In a real scenario, you would connect this server to a transport.
  // e.g. await server.connect(StdioServerTransport());
}
