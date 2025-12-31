<?php

require_once __DIR__ . '/vendor/autoload.php';

use Dotenv\Dotenv;
use Mcp\Server\Builder;
use Mcp\Server\Transport\StdioTransport;
use Monolog\Handler\StreamHandler;
use Monolog\Logger;
use Monolog\Formatter\JsonFormatter;
use Psr\Log\LogLevel;
use Google\Cloud\Firestore\FirestoreClient;
use Google\Cloud\Core\Timestamp;
use Mcp\Exception\ToolCallException;
use Mcp\Schema\ToolAnnotations;

// Load .env variables if present
$dotenv = Dotenv::createImmutable(__DIR__);
$dotenv->safeLoad();

// Set up logging
ini_set('display_errors', '0');
ini_set('log_errors', '1');
ini_set('error_log', 'php://stderr');
error_reporting(E_ALL);

$logger = new Logger('mcp');
$handler = new StreamHandler('php://stderr', LogLevel::INFO);
$handler->setFormatter(new JsonFormatter());
$logger->pushHandler($handler);

// Global state
$firestore = null;
$dbRunning = false;

// Initialize Firestore
try {
    // Attempt to initialize Firestore. 
    // Relies on GOOGLE_APPLICATION_CREDENTIALS or implicit env.
    $firestore = new FirestoreClient();
    $dbRunning = true;
    $logger->info("Firestore client initialized");
} catch (\Throwable $e) {
    $logger->error("Failed to initialize Firestore", ['error' => $e->getMessage()]);
    $dbRunning = false;
}

// Helper Functions

function docToProduct($doc) {
    $data = $doc->data();
    if (!$data) {
        throw new \Exception("Document data is empty");
    }
    
    $product = [
        'id' => $doc->id(),
        'name' => $data['name'] ?? '',
        'price' => $data['price'] ?? 0,
        'quantity' => $data['quantity'] ?? 0,
        'imgfile' => $data['imgfile'] ?? '',
        'timestamp' => $data['timestamp'] ?? null,
        'actualdateadded' => $data['actualdateadded'] ?? null,
    ];

    // Convert Timestamps to ISO strings for JSON
    if ($product['timestamp'] instanceof Timestamp) {
        $product['timestamp'] = $product['timestamp']->get()->format(DATE_ATOM);
    } elseif ($product['timestamp'] instanceof \DateTimeInterface) {
        $product['timestamp'] = $product['timestamp']->format(DATE_ATOM);
    }

    if ($product['actualdateadded'] instanceof Timestamp) {
        $product['actualdateadded'] = $product['actualdateadded']->get()->format(DATE_ATOM);
    } elseif ($product['actualdateadded'] instanceof \DateTimeInterface) {
        $product['actualdateadded'] = $product['actualdateadded']->format(DATE_ATOM);
    }

    return $product;
}

function addOrUpdateFirestore(FirestoreClient $firestore, array $product) {
    $collection = $firestore->collection('inventory');
    $query = $collection->where('name', '=', $product['name']);
    $documents = $query->documents();
    
    $found = false;
    foreach ($documents as $doc) {
        $found = true;
        $doc->reference()->set($product, ['merge' => true]);
    }
    
    if (!$found) {
        $collection->add($product);
    }
}

function cleanFirestoreCollection(FirestoreClient $firestore, Logger $logger) {
    $logger->info("Cleaning Firestore collection...");
    $documents = $firestore->collection('inventory')->documents();
    
    $bulkWriter = $firestore->bulkWriter();
    foreach ($documents as $doc) {
        $bulkWriter->delete($doc->reference());
    }
    $bulkWriter->flush();
    $logger->info("Firestore collection cleaned.");
}

function initFirestoreCollection(FirestoreClient $firestore, Logger $logger) {
    $oldProducts = [
        "Apples", "Bananas", "Milk", "Whole Wheat Bread", "Eggs", "Cheddar Cheese",
        "Whole Chicken", "Rice", "Black Beans", "Bottled Water", "Apple Juice",
        "Cola", "Coffee Beans", "Green Tea", "Watermelon", "Broccoli",
        "Jasmine Rice", "Yogurt", "Beef", "Shrimp", "Walnuts",
        "Sunflower Seeds", "Fresh Basil", "Cinnamon",
    ];

    foreach ($oldProducts as $productName) {
        $oldProduct = [
            'name' => $productName,
            'price' => rand(1, 10),
            'quantity' => rand(1, 500),
            'imgfile' => "product-images/" . strtolower(str_replace(' ', '', $productName)) . ".png",
            // Approx logic for date: now - random(0 to 1 year) - 3 months
            'timestamp' => new Timestamp(new \DateTimeImmutable("-" . (rand(0, 365) + 90) . " days")), 
            'actualdateadded' => new Timestamp(new \DateTimeImmutable()),
        ];
        // Note: The timestamp logic in TS was: Date.now() - rand(31536000000) - 7776000000
        // 31536000000 ms is approx 1 year. 7776000000 ms is approx 90 days.
        
        $logger->info("⬆️ Adding (or updating) product in firestore: " . $oldProduct['name']);
        addOrUpdateFirestore($firestore, $oldProduct);
    }

    $recentProducts = [
        "Parmesan Crisps", "Pineapple Kombucha", "Maple Almond Butter",
        "Mint Chocolate Cookies", "White Chocolate Caramel Corn", "Acai Smoothie Packs",
        "Smores Cereal", "Peanut Butter and Jelly Cups",
    ];

    foreach ($recentProducts as $productName) {
        $recent = [
            'name' => $productName,
            'price' => rand(1, 10),
            'quantity' => rand(1, 100),
            'imgfile' => "product-images/" . strtolower(str_replace(' ', '', $productName)) . ".png",
            'timestamp' => new Timestamp(new \DateTimeImmutable("-" . rand(0, 6) . " days")),
            'actualdateadded' => new Timestamp(new \DateTimeImmutable()),
        ];
        $logger->info("🆕 Adding (or updating) product in firestore: " . $recent['name']);
        addOrUpdateFirestore($firestore, $recent);
    }

    $recentProductsOutOfStock = ["Wasabi Party Mix", "Jalapeno Seasoning"];
    foreach ($recentProductsOutOfStock as $productName) {
        $oosProduct = [
            'name' => $productName,
            'price' => rand(1, 10),
            'quantity' => 0,
            'imgfile' => "product-images/" . strtolower(str_replace(' ', '', $productName)) . ".png",
            'timestamp' => new Timestamp(new \DateTimeImmutable("-" . rand(0, 6) . " days")),
            'actualdateadded' => new Timestamp(new \DateTimeImmutable()),
        ];
        $logger->info("😱 Adding (or updating) out of stock product in firestore: " . $oosProduct['name']);
        addOrUpdateFirestore($firestore, $oosProduct);
    }
}


// Initialize MCP Server Builder
$builder = new Builder();
$builder->setServerInfo('inventory-server', '1.0.0');
$builder->setLogger($logger);

// --- Register Tools ---

// 1. get_products
$builder->addTool(
    function () use (&$firestore, &$dbRunning) {
        if (!$dbRunning) {
            throw new ToolCallException("Inventory database is not running.");
        }
        $products = $firestore->collection('inventory')->documents();
        $productsArray = [];
        foreach ($products as $doc) {
            $productsArray[] = docToProduct($doc);
        }
        return json_encode($productsArray, JSON_PRETTY_PRINT);
    },
    'get_products',
    'Get a list of all products from the inventory database',
    new ToolAnnotations(title: 'Get all products')
);

// 2. get_product_by_id
$builder->addTool(
    function (string $id) use (&$firestore, &$dbRunning) {
        if (!$dbRunning) {
            throw new ToolCallException("Inventory database is not running.");
        }
        $doc = $firestore->collection('inventory')->document($id)->snapshot();
        if (!$doc->exists()) {
             throw new ToolCallException("Product not found.");
        }
        return json_encode(docToProduct($doc), JSON_PRETTY_PRINT);
    },
    'get_product_by_id',
    'Get a single product from the inventory database by its ID',
    new ToolAnnotations(title: 'Get product by ID'),
    [
        'type' => 'object',
        'properties' => [
            'id' => [
                'type' => 'string',
                'description' => 'The ID of the product to get'
            ]
        ],
        'required' => ['id']
    ]
);

// 3. seed
$builder->addTool(
    function () use (&$firestore, &$dbRunning, $logger) {
        if (!$dbRunning) {
            throw new ToolCallException("Inventory database is not running.");
        }
        initFirestoreCollection($firestore, $logger);
        return "Database seeded successfully.";
    },
    'seed',
    'Seed the inventory database with products.',
    new ToolAnnotations(title: 'Seed database')
);

// 4. reset
$builder->addTool(
    function () use (&$firestore, &$dbRunning, $logger) {
         if (!$dbRunning) {
            throw new ToolCallException("Inventory database is not running.");
        }
        cleanFirestoreCollection($firestore, $logger);
        return "Database reset successfully.";
    },
    'reset',
    'Clears all products from the inventory database.',
    new ToolAnnotations(title: 'Reset database')
);

// 5. get_root (replaces existing greeting)
$builder->addTool(
    function () {
        return "🍎 Hello! This is the Cymbal Superstore Inventory API.";
    },
    'get_root',
    'Get a greeting from the Cymbal Superstore Inventory API.',
    new ToolAnnotations(title: 'Get root')
);

// 6. check_db
$builder->addTool(
    function () use (&$dbRunning) {
        return "Database running: " . ($dbRunning ? 'true' : 'false');
    },
    'check_db',
    'Checks if the inventory database is running.',
    new ToolAnnotations(title: 'Check DB Status')
);

$server = $builder->build();

// Explicitly use stdio transport
$transport = new StdioTransport();
$server->run($transport);