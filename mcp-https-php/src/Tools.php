<?php

declare(strict_types=1);

namespace App;

use Psr\Log\LoggerInterface;

class Tools
{
    public function __construct(private LoggerInterface $logger)
    {
    }

    /**
     * Get a greeting from a local http server.
     */
    public function greet(string $name): string
    {
        $this->logger->debug("Executed greet tool");
        return "Hello, " . $name . "!";
    }

    /**
     * Get the current system time.
     */
    public function getTime(): string
    {
        $this->logger->debug("Executed get_time tool");
        return date('Y-m-d H:i:s');
    }

    /**
     * Get system specifications and information.
     */
    public function getSystemSpecs(): string
    {
        $this->logger->debug("Executed get_system_specs tool");
        return json_encode([
            'os' => php_uname(),
            'php_version' => phpversion(),
            'architecture' => php_uname('m'),
            'hostname' => gethostname(),
        ], JSON_PRETTY_PRINT | JSON_THROW_ON_ERROR);
    }
}
