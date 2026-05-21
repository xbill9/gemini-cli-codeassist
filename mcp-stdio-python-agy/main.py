# main.py

import logging
import os
import platform
import sys
from datetime import datetime, timezone
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
mcp = FastMCP("hello-world-server")


@mcp.tool()
def greet(param: str) -> str:
    """
    Get a greeting from a local stdio server.
    """
    logger.debug("Executed greet tool")
    # FastMCP automatically wraps the return value in TextContent
    return param


@mcp.tool()
def get_system_time() -> str:
    """
    Get the current system time in ISO-8601 format and human-readable format.
    """
    logger.debug("Executed get_system_time tool")
    now = datetime.now()
    now_utc = datetime.now(timezone.utc)
    local_str = (
        f"Local Time: {now.isoformat()} ({now.strftime('%Y-%m-%d %H:%M:%S')})"
    )
    utc_str = f"UTC Time: {now_utc.isoformat()}"
    return f"{local_str}\n{utc_str}"


@mcp.tool()
def get_system_info() -> str:
    """
    Get current system information including OS details, CPU count, and
    memory usage.
    """
    logger.debug("Executed get_system_info tool")

    # OS details
    os_name = platform.system()
    os_release = platform.release()
    os_version = platform.version()
    architecture = platform.machine()

    # CPU count
    cpu_count = os.cpu_count() or "Unknown"

    # Memory details (specific to Linux or general fallback)
    mem_details = "Memory information not available."
    if os_name == "Linux":
        try:
            with open("/proc/meminfo", "r") as f:
                lines = f.readlines()
            mem_info = {}
            for line in lines:
                parts = line.split(":")
                if len(parts) == 2:
                    mem_info[parts[0].strip()] = parts[1].strip()

            total = mem_info.get("MemTotal", "Unknown")
            free = mem_info.get("MemFree", "Unknown")
            available = mem_info.get("MemAvailable", "Unknown")
            mem_details = (
                f"Total: {total}, Free: {free}, "
                f"Available: {available}"
            )
        except Exception as e:
            mem_details = f"Error reading /proc/meminfo: {str(e)}"
    else:
        # Fallback to sysconf (POSIX systems)
        try:
            page_size = os.sysconf("SC_PAGE_SIZE")
            total_pages = os.sysconf("SC_PHYS_PAGES")
            avail_pages = os.sysconf("SC_AVPHYS_PAGES")
            total_mem = (page_size * total_pages) / (1024 * 1024 * 1024)  # GB
            avail_mem = (page_size * avail_pages) / (1024 * 1024 * 1024)  # GB
            mem_details = (
                f"Total: {total_mem:.2f} GB, "
                f"Available: {avail_mem:.2f} GB"
            )
        except Exception:
            pass

    # Python details
    python_version = sys.version.split()[0]

    info = [
        f"Operating System: {os_name}",
        f"Release/Kernel: {os_release}",
        f"Version: {os_version}",
        f"Architecture: {architecture}",
        f"CPU Cores: {cpu_count}",
        f"Memory Info: {mem_details}",
        f"Python Version: {python_version}",
    ]

    return "\n".join(info)


if __name__ == "__main__":
    # Explicitly use stdio transport
    mcp.run(transport="stdio")
