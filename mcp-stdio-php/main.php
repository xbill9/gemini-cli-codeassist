<?php

require_once __DIR__ . '/vendor/autoload.php';

use Mcp\Server\Builder;
use Mcp\Server\Transport\StdioTransport;
use Monolog\Handler\StreamHandler;
use Monolog\Logger;
use Monolog\Formatter\JsonFormatter;
use Psr\Log\LogLevel;

// Set up logging to match index.ts/main.py behavior
$logger = new Logger('mcp');
$handler = new StreamHandler('php://stderr', LogLevel::INFO);
$handler->setFormatter(new JsonFormatter());
$logger->pushHandler($handler);

// Initialize MCP Server
$server = (new Builder())
    ->setServerInfo('hello-world-server', '1.0.0')
    ->setLogger($logger)
    ->addTool(
        function (string $param) use ($logger): string {
            /**
             * Get a greeting from a local stdio server.
             */
            $logger->debug("Executed greet tool");
            return $param;
        },
        'greet',
        'Get a greeting from a local stdio server.'
    )
    ->build();

// Explicitly use stdio transport
$transport = new StdioTransport();
$server->run($transport);
