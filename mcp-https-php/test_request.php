<?php
require_once __DIR__.'/vendor/autoload.php';
use Http\Discovery\Psr17Factory;
$factory = new Psr17Factory();
$request = $factory->createServerRequestFromGlobals();
echo "Method: " . $request->getMethod() . "\n";
echo "URI: " . (string)$request->getUri() . "\n";

