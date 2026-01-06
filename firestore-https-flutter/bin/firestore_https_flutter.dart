import 'package:firestore_https_flutter/firestore_https_flutter.dart';
import 'package:args/args.dart';
import 'package:firedart/firedart.dart';
import 'package:dotenv/dotenv.dart';

void main(List<String> arguments) async {
  final parser = ArgParser()
    ..addOption('port',
        abbr: 'p', defaultsTo: '8080', help: 'Port for HTTP transport')
    ..addOption('host', defaultsTo: 'localhost', help: 'Host for HTTP transport')
    ..addOption('path', defaultsTo: '/mcp', help: 'Path for HTTP transport');

  final results = parser.parse(arguments);

  // Initialize Environment and Firestore
  final env = DotEnv(includePlatformEnvironment: true)..load();
  final projectId = env['GOOGLE_CLOUD_PROJECT'];
  if (projectId != null) {
    Firestore.initialize(projectId, useApplicationDefaultAuth: true);
    log('INFO', 'Firestore initialized', {'projectId': projectId});
  } else {
    log('WARNING', 'GOOGLE_CLOUD_PROJECT not found in environment');
  }

  final port = int.parse(results['port']);
  final host = results['host'];
  final path = results['path']!;

  final server = StreamableMcpServer(
    serverFactory: (sessionId) => createServer(),
    host: host,
    port: port,
    path: path,
  );
  await server.start();
  log('INFO', 'Server started',
      {'transport': 'http', 'host': host, 'port': port, 'path': path});
}