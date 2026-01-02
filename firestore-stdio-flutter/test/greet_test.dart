import 'package:test/test.dart';
import 'package:firestore_stdio_flutter/tools.dart';
import 'package:mcp_dart/mcp_dart.dart';

void main() {
  test('get_root returns greeting', () async {
    final args = <String, dynamic>{};
    final result = await getRootHandler(args, null);
    expect(result.isError, isFalse);
    expect(result.content, isNotEmpty);
    expect((result.content.first as TextContent).text, contains("Cymbal Superstore Inventory API"));
  });

  test('check_db returns status', () async {
    final args = <String, dynamic>{};
    final result = await checkDbHandler(args, null);
    expect(result.isError, isFalse);
    expect((result.content.first as TextContent).text, contains("Database running:"));
  });
}