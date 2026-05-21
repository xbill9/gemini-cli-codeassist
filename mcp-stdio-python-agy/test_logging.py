import pytest
import logging
from io import StringIO
from main import logger, greet
from pythonjsonlogger.json import JsonFormatter


@pytest.fixture
def caplog_stream():
    # Create a StringIO object to capture logs
    log_stream = StringIO()

    # Create a new handler that writes to our StringIO object
    # Use the same formatter as in main.py
    formatter = JsonFormatter()
    handler = logging.StreamHandler(log_stream)
    handler.setFormatter(formatter)
    handler.setLevel(logging.DEBUG)  # Capture all levels for testing

    # Get the root logger and remove existing handlers
    # This is crucial because main.py configures the logger at import time
    original_handlers = logger.handlers[:]  # Copy list
    for h in original_handlers:
        logger.removeHandler(h)

    logger.addHandler(handler)
    original_level = logger.level
    # Set logger to DEBUG to capture all messages from the main logger
    logger.setLevel(logging.DEBUG)

    yield log_stream  # Yield the StringIO object itself

    # Clean up: restore original handlers and level
    logger.removeHandler(handler)
    for h in original_handlers:
        logger.addHandler(h)
    logger.setLevel(original_level)


def test_info_to_stderr(caplog_stream):
    logger.info("This is an info message.")
    captured_logs = caplog_stream.getvalue()
    assert "This is an info message." in captured_logs


def test_error_to_stderr(caplog_stream):
    logger.error("This is an error message.")
    captured_logs = caplog_stream.getvalue()
    assert "This is an error message." in captured_logs


def test_debug_is_captured_in_test(caplog_stream):
    logger.debug("This is a debug message.")
    captured_logs = caplog_stream.getvalue()
    assert "This is a debug message." in captured_logs


def test_greet_tool_logging(caplog_stream):
    # Simulate a call to the greet tool
    greet("test_param")
    captured_logs = caplog_stream.getvalue()
    # The greet tool now logs at DEBUG level, which should be captured
    assert "Executed greet tool" in captured_logs
