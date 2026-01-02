import 'package:test/test.dart';
import 'package:mcp_stdio_flutter/tools.dart';
import 'package:mcp_dart/mcp_dart.dart';

void main() {
  test('greet returns param', () async {
    final args = {'param': 'hello world'};
    final result = await greetHandler(args, null);
    expect(result.isError, isFalse);
    expect(result.content, isNotEmpty);
    expect((result.content.first as TextContent).text, 'hello world');
  });

  test('greet handles empty string', () async {
    final args = {'param': ''};
    final result = await greetHandler(args, null);
    expect(result.isError, isFalse);
    expect((result.content.first as TextContent).text, '');
  });

  test('greet handles missing param', () async {
    final args = <String, dynamic>{}; // No param
    final result = await greetHandler(args, null);
    expect(result.isError, isTrue);
    expect((result.content.first as TextContent).text, contains("Missing 'param' argument"));
  });
}
