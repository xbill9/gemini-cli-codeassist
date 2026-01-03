import 'package:test/test.dart';
import 'package:firestore_https_flutter/tools.dart';

void main() {
  test('greet returns name', () async {
    final args = {'name': 'hello world'};
    final result = await greetHandler(args, null);
    expect(result.isError, isFalse);
    expect(result.content, isNotEmpty);
    expect((result.content.first as TextContent).text, 'Hello, hello world!');
  });

  test('greet handles empty string', () async {
    final args = {'name': ''};
    final result = await greetHandler(args, null);
    expect(result.isError, isFalse);
    expect((result.content.first as TextContent).text, 'Hello, !');
  });

  test('greet handles missing name', () async {
    final args = <String, dynamic>{}; // No name
    final result = await greetHandler(args, null);
    expect(result.isError, isTrue);
    expect((result.content.first as TextContent).text, contains("Missing 'name' argument"));
  });
}