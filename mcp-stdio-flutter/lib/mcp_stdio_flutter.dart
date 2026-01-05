import 'package:mcp_dart/mcp_dart.dart';

Future<CallToolResult> greetHandler(Map<String, dynamic> args, dynamic extra) async {
  final param = args['param'] as String?;
  
  if (param != null) {
    return CallToolResult(
      content: [
        TextContent(
          text: param
        )
      ]
    );
  } else {
      return CallToolResult(
      isError: true,
      content: [
        TextContent(
          text: "Missing 'param' argument"
        )
      ]
      );
  }
}
