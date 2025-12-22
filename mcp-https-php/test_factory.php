<?php
require_once __DIR__.'/vendor/autoload.php';
use Http\Discovery\Psr17Factory;
try {
    $factory = new Psr17Factory();
    echo "Class: " . get_class($factory) . "\n";
    if (method_exists($factory, 'createServerRequestFromGlobals')) {
        echo "Method exists\n";
    } else {
        echo "Method does NOT exist\n";
    }
} catch (\Throwable $e) {
    echo "Error: " . $e->getMessage() . "\n";
}

