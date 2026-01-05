import 'package:mcp_dart/mcp_dart.dart';

/// Handler for the 'greet' tool.
///
/// Takes a 'param' argument and returns it as a text response.
Future<CallToolResult> greetHandler(
  Map<String, dynamic> args,
  dynamic extra,
) async {
  final param = args['param'] as String?;

  if (param != null) {
    return CallToolResult(content: [TextContent(text: param)]);
  } else {
    return const CallToolResult(
      isError: true,
      content: [
        TextContent(text: "Missing 'param' argument"),
      ],
    );
  }
}
