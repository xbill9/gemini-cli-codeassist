#!/usr/bin/env perl
use Mojo::Base -strict, -signatures;
use MCP::Server;
use Mojo::Log;
use JSON::MaybeXS;
use lib 'lib';
use GoogleAuth;
use FirestoreClient;

# Initialize JSON encoder for logging
my $json = JSON::MaybeXS->new->utf8->canonical;

# Setup Logger to mimic python-json-logger
# We will use Mojo::Log but format the output as JSON to STDERR
my $log = Mojo::Log->new( level => 'info', handle => \*STDERR );
$log->format(
    sub ( $time, $level, @lines ) {
        my $dt = Mojo::Date->new($time)->to_datetime;
        return $json->encode(
            {
                asctime   => $dt,
                levelname => uc($level),
                message   => join( ' ', @lines ),
                name      => 'root'
            }
        ) . "\n";
    }
);

# Redirect standard warnings to JSON logger
$SIG{__WARN__} = sub {
    my $msg = shift;
    chomp $msg;
    $log->warn($msg);
};

my $auth = GoogleAuth->new;
my $firestore;

eval {
    # Initialize Firestore Client
    # We need to make sure we have authentication
    # Check if we can get project ID (this verifies auth to some extent)
    my $project_id = $auth->get_project_id;
    $log->info("Initializing Firestore for project: $project_id");

    $firestore = FirestoreClient->new( auth => $auth, log => $log );
    $log->info("Firestore initialized successfully");
};
if ($@) {
    $log->warn("Failed to initialize Firestore: $@");
    $log->warn("Firestore tools will be unavailable.");
}

$log->info("Starting MCP server");
my $server = MCP::Server->new;

$server->tool(
    name         => 'greet',
    description  => 'Get a greeting from a local stdio server.',
    input_schema => {
        type       => 'object',
        properties => {
            param => { type => 'string' }
        },
        required => ['param']
    },
    code => sub ( $tool, $args ) {
        $log->debug("Executed greet tool");
        return $args->{param};
    }
);

if ($firestore) {
    $server->tool(
        name         => 'list_products',
        description  => 'List all products in the inventory',
        input_schema => {
            type       => 'object',
            properties => {},
        },
        code => sub ( $tool, $args ) {
            $log->debug("Executed list_products");
            my $products = $firestore->list_products;
            $log->info("Returning " . scalar(@$products) . " products");
            my $result = $tool->structured_result({ products => $products });
            $log->info("Result type: " . ref($result));
            return $result;
        }
    );

    $server->tool(
        name         => 'get_product',
        description  => 'Get a product by ID',
        input_schema => {
            type       => 'object',
            properties => {
                id => { type => 'string' }
            },
            required => ['id']
        },
        code => sub ( $tool, $args ) {
            $log->debug( "Executed get_product: " . $args->{id} );
            return $tool->structured_result( $firestore->get_product( $args->{id} ) );
        }
    );

    $server->tool(
        name         => 'add_product',
        description  => 'Add a new product',
        input_schema => {
            type       => 'object',
            properties => {
                name     => { type => 'string' },
                price    => { type => 'number' },
                quantity => { type => 'integer' },
                imgfile  => { type => 'string' },
            },
            required => [ 'name', 'price', 'quantity', 'imgfile' ]
        },
        code => sub ( $tool, $args ) {
            $log->debug("Executed add_product");
            my $now = Mojo::Date->new->to_datetime;

# Ensure format compatible with Firestore expectations if needed, but Mojo::Date is RFC3339 compatible-ish
# Models.swift uses ISO8601 with fractional seconds. Mojo::Date uses RFC 7231 by default but to_datetime is ISO 8601.

            my $product = {
                name            => $args->{name},
                price           => $args->{price},
                quantity        => $args->{quantity},
                imgfile         => $args->{imgfile},
                timestamp       => $now . "Z",          # Simple approximation
                actualdateadded => $now . "Z",
            };
            return $firestore->add_product($product);
        }
    );

    $server->tool(
        name         => 'update_product',
        description  => 'Update an existing product',
        input_schema => {
            type       => 'object',
            properties => {
                id       => { type => 'string' },
                name     => { type => 'string' },
                price    => { type => 'number' },
                quantity => { type => 'integer' },
                imgfile  => { type => 'string' },
            },
            required => [ 'id', 'name', 'price', 'quantity', 'imgfile' ]
        },
        code => sub ( $tool, $args ) {
            $log->debug( "Executed update_product: " . $args->{id} );
            my $now     = Mojo::Date->new->to_datetime;
            my $product = {
                name            => $args->{name},
                price           => $args->{price},
                quantity        => $args->{quantity},
                imgfile         => $args->{imgfile},
                timestamp       => $now . "Z",
                actualdateadded => $now . "Z"
                , # Preserving original creation time might be better but simplification for now
            };
            return $firestore->update_product( $args->{id}, $product );
        }
    );

    $server->tool(
        name         => 'delete_product',
        description  => 'Delete a product by ID',
        input_schema => {
            type       => 'object',
            properties => {
                id => { type => 'string' }
            },
            required => ['id']
        },
        code => sub ( $tool, $args ) {
            $log->debug( "Executed delete_product: " . $args->{id} );
            return $firestore->delete_product( $args->{id} );
        }
    );

    $server->tool(
        name         => 'find_products',
        description  => 'Find products by name',
        input_schema => {
            type       => 'object',
            properties => {
                name => { type => 'string' }
            },
            required => ['name']
        },
        code => sub ( $tool, $args ) {
            $log->debug( "Executed find_products: " . $args->{name} );
            return $tool->structured_result( { products => $firestore->find_products( $args->{name} ) } );
        }
    );

    $server->tool(
        name         => 'batch_delete',
        description  => 'Delete multiple products by ID',
        input_schema => {
            type       => 'object',
            properties => {
                ids => { type => 'array', items => { type => 'string' } }
            },
            required => ['ids']
        },
        code => sub ( $tool, $args ) {
            $log->debug("Executed batch_delete");
            return $firestore->batch_delete( $args->{ids} );
        }
    );
}

# Explicitly use stdio transport
$log->info("Server entering stdio loop");
$server->to_stdio;

