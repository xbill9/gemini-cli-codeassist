# main.py

import logging
import sys

from pythonjsonlogger.json import JsonFormatter
from fastmcp import FastMCP
from starlette.responses import JSONResponse

# Set up logging
logger = logging.getLogger()
logger.setLevel(logging.INFO)  # Set the root logger level
formatter = JsonFormatter()

# Handler for all levels to stderr
stderr_handler = logging.StreamHandler(sys.stderr)
stderr_handler.setFormatter(formatter)
stderr_handler.setLevel(logging.INFO)  # Capture all levels from INFO up
logger.addHandler(stderr_handler)

# Initialize FastMCP server with HTTP transport
mcp = FastMCP(
    "hello-world-server"
)


# health check on standard http endpoint
@mcp.custom_route("/health", methods=["GET"])
async def health_check(request):
    return JSONResponse({"status": "healthy", "service": "mcp-server"})


@mcp.tool()
def greet(param: str) -> str:
    """
    Get a greeting from a local stdio server.
    """
    logger.debug("Executed greet tool")
    # FastMCP automatically wraps the return value in TextContent
    return param


if __name__ == "__main__":
    logger.debug("Starting MCP server")
    mcp.run(transport="http", host="0.0.0.0", port=8080)
