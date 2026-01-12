#!/usr/bin/env perl
use Mojo::Base -strict, -signatures;
use lib 'lib';
use GoogleAuth;
use FirestoreClient;
use Mojo::Log;
use JSON::MaybeXS;
use Mojo::Date;

# Initialize JSON encoder
my $json = JSON::MaybeXS->new->utf8->canonical;

# Setup Logger to mimic python-json-logger
my $log = Mojo::Log->new( level => 'info', handle => \*STDERR );
$log->format(
    sub ( $time, $level, @lines ) {
        my $dt = Mojo::Date->new($time)->to_datetime;
        return $json->encode(
            {
                asctime   => $dt,
                levelname => uc($level),
                message   => join( ' ', @lines ),
                name      => 'test_firestore'
            }
        ) . "\n";
    }
);

my $auth = GoogleAuth->new;

eval {
    my $project_id = $auth->get_project_id;
    $log->info("Project ID: $project_id");
    
    my $firestore = FirestoreClient->new(auth => $auth, log => $log);
    my $products = $firestore->list_products;
    
    $log->info("Products found: " . scalar(@$products));
    foreach my $p (@$products) {
        $log->info(" - $p->{name} ($p->{id})");
    }
};
if ($@) {
    $log->error("Error: $@");
}

