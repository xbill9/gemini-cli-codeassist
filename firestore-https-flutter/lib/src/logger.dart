import 'dart:convert';
import 'dart:io';

/// Logs a message to stderr in JSON format.
void log(String level, String message, [Map<String, dynamic>? context]) {
  final entry = {
    'timestamp': DateTime.now().toIso8601String(),
    'level': level,
    'message': message,
    if (context != null) 'context': context,
  };
  stderr.writeln(jsonEncode(entry));
}
