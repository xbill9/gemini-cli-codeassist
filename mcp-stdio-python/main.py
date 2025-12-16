# main.py

import logging
import sys
from pythonjsonlogger.json import JsonFormatter
from mcp.server.fastmcp import FastMCP

# Set up logging to match index.ts behavior
logger = logging.getLogger()
logger.setLevel(logging.INFO)  # Set the root logger level

formatter = JsonFormatter()

# Handler for all levels to stderr
stderr_handler = logging.StreamHandler(sys.stderr)
stderr_handler.setFormatter(formatter)
stderr_handler.setLevel(logging.INFO)  # Capture all levels from INFO up
logger.addHandler(stderr_handler)

# Initialize FastMCP server
# Matching name from index.ts: "hello-world-server"
mcp = FastMCP("hello-world-server")


@mcp.tool()
def greet(param: str) -> str:
    """
    Get a greeting from a local stdio server.
    """
    logger.debug("Executed greet tool")
    # FastMCP automatically wraps the return value in TextContent
    return param


if __name__ == "__main__":
    # Explicitly use stdio transport
    mcp.run(transport="stdio")
