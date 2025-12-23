<?php

declare(strict_types=1);

require_once __DIR__ . '/vendor/autoload.php';

use App\Tools;
use Http\Discovery\Psr17Factory;
use Laminas\HttpHandlerRunner\Emitter\SapiEmitter;
use Mcp\Server\Builder;
use Mcp\Server\Session\FileSessionStore;
use Mcp\Server\Transport\StreamableHttpTransport;
use Monolog\Handler\StreamHandler;
use Monolog\Logger;
use Monolog\Formatter\JsonFormatter;
use Psr\Http\Message\ResponseInterface;
use Psr\Log\LogLevel;

if ('cli' === \PHP_SAPI) {
    fwrite(STDERR, "This MCP server is designed to run over HTTP.\nUse 'make run' or 'php -S 0.0.0.0:8080 main.php' to start it.\n");
    exit(1);
}

// Set up logging to match index.ts/main.py behavior
$logger = new Logger('mcp');
$handler = new StreamHandler('php://stderr', LogLevel::INFO);
$handler->setFormatter(new JsonFormatter());
$logger->pushHandler($handler);

$tools = new Tools($logger);

// Initialize MCP Server
$server = (new Builder())
    ->setServerInfo('hello-world-server', '1.0.0')
    ->setLogger($logger)
    ->setSession(new FileSessionStore(__DIR__ . '/sessions'))
    ->addTool(
        fn (string $name) => $tools->greet($name),
        'greet',
        'Get a greeting from a local MCP http server.'
    )
    ->addTool(
        fn () => $tools->getTime(),
        'get_time',
        'Get the current system time.'
    )
    ->addTool(
        fn () => $tools->getSystemSpecs(),
        'get_system_specs',
        'Get system specifications and information.'
    )
    ->build();

// Select transport based on SAPI
$transport = new StreamableHttpTransport(
    (new Psr17Factory())->createServerRequestFromGlobals(),
    logger: $logger
);

/** @var ResponseInterface $result */
$result = $server->run($transport);

(new SapiEmitter())->emit($result);
exit(0);