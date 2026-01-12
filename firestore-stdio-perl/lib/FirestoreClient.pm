package FirestoreClient;
use Mojo::Base -base, -signatures;
use Mojo::UserAgent;
use Mojo::JSON qw(decode_json encode_json true false);
use Mojo::Date;
use Mojo::URL;

has auth => sub {
    die "auth is required";
};    # Expected to respond to get_project_id and get_access_token
has ua  => sub { Mojo::UserAgent->new( inactivity_timeout => 30 ) };
has log => sub { Mojo::Log->new };

sub _get_base_url ($self) {
    my $project_id = $self->auth->get_project_id;
    return
"https://firestore.googleapis.com/v1/projects/$project_id/databases/(default)/documents";
}

sub _get_headers ($self) {
    my $token = $self->auth->get_access_token;
    return {
        'Authorization' => "Bearer $token",
        'Content-Type'  => 'application/json'
    };
}

# MARK: - CRUD

sub list_products ($self) {
    my $base_url = $self->_get_base_url;
    my $url      = "$base_url/inventory";

    my $tx = $self->ua->get( $url => $self->_get_headers );

    if ( my $err = $tx->error ) {
        my $msg = $tx->res->body || $err->{message};
        die "Firestore API Error: " . $tx->res->code . " - " . $msg;
    }

    my $data      = $tx->res->json;
    my $documents = $data->{documents} // [];

    my @products;
    foreach my $doc (@$documents) {
        my $p = $self->_document_to_product($doc);
        push @products, $p if $p;
    }
    return \@products;
}

sub get_product ( $self, $id ) {
    my $base_url = $self->_get_base_url;
    my $url      = "$base_url/inventory/$id";

    my $tx = $self->ua->get( $url => $self->_get_headers );

    if ( my $err = $tx->error ) {
        return undef if $tx->res->code == 404;
        my $msg = $tx->res->body || $err->{message};
        die "Firestore API Error: " . $tx->res->code . " - " . $msg;
    }

    return $self->_document_to_product( $tx->res->json );
}

sub add_product ( $self, $product ) {
    my $base_url = $self->_get_base_url;
    my $url      = "$base_url/inventory";

    my $doc_fields = $self->_product_to_firestore_fields($product);
    my $body       = { fields => $doc_fields };

    my $tx = $self->ua->post( $url => $self->_get_headers => json => $body );

    if ( my $err = $tx->error ) {
        my $msg = $tx->res->body || $err->{message};
        die "Firestore API Error: " . $tx->res->code . " - " . $msg;
    }
    return 1;
}

sub update_product ( $self, $id, $product ) {
    my $base_url = $self->_get_base_url;
    my $url      = "$base_url/inventory/$id";

    my $doc_fields = $self->_product_to_firestore_fields($product);
    my $body       = { fields => $doc_fields };

    # Using PATCH
    my $tx = $self->ua->patch( $url => $self->_get_headers => json => $body );

    if ( my $err = $tx->error ) {
        my $msg = $tx->res->body || $err->{message};
        die "Firestore API Error: " . $tx->res->code . " - " . $msg;
    }
    return 1;
}

sub delete_product ( $self, $id ) {
    my $base_url = $self->_get_base_url;
    my $url      = "$base_url/inventory/$id";

    my $tx = $self->ua->delete( $url => $self->_get_headers );

    if ( my $err = $tx->error ) {
        my $msg = $tx->res->body || $err->{message};
        die "Firestore API Error: " . $tx->res->code . " - " . $msg;
    }
    return 1;
}

# MARK: - Query

sub find_products ( $self, $name ) {
    my $base_url = $self->_get_base_url;
    my $url      = "$base_url:runQuery";

    my $query = {
        structuredQuery => {
            from  => [ { collectionId => 'inventory' } ],
            where => {
                fieldFilter => {
                    field => { fieldPath => 'name' },
                    op    => 'EQUAL',
                    value => { stringValue => $name },
                }
            }
        }
    };

    my $tx = $self->ua->post( $url => $self->_get_headers => json => $query );

    if ( my $err = $tx->error ) {
        my $msg = $tx->res->body || $err->{message};
        die "Firestore API Error: " . $tx->res->code . " - " . $msg;
    }

    my $items = $tx->res->json;    # Array of results
    my @products;

# runQuery returns a list of objects usually containing 'document' or 'readTime'
# Wait, in Swift it decodes [QueryResponseItem].
# In Mojo, $items should be an arrayref if the response is a JSON array.
# The response from runQuery is indeed a stream of objects in a JSON array.

    if ( ref $items eq 'ARRAY' ) {
        foreach my $item (@$items) {
            next unless $item->{document};
            my $p = $self->_document_to_product( $item->{document} );
            push @products, $p if $p;
        }
    }

    return \@products;
}

# MARK: - Batch

sub batch_delete ( $self, $ids ) {
    return unless $ids && @$ids;

    my $project_id = $self->auth->get_project_id;
    my $url =
"https://firestore.googleapis.com/v1/projects/$project_id/databases/(default)/documents:commit";

    my @writes;
    foreach my $id (@$ids) {
        push @writes,
          { delete =>
              "projects/$project_id/databases/(default)/documents/inventory/$id"
          };
    }

    my $body = { writes => \@writes };

    my $tx = $self->ua->post( $url => $self->_get_headers => json => $body );

    if ( my $err = $tx->error ) {
        my $msg = $tx->res->body || $err->{message};
        die "Firestore API Error: " . $tx->res->code . " - " . $msg;
    }
    return 1;
}

# MARK: - Helpers

sub _document_to_product ( $self, $doc ) {
    my $fields    = $doc->{fields};
    my $name_path = $doc->{name};
    return undef unless $fields && $name_path;

    my ($id) = $name_path =~ m{/([^/]+)$};

    my $name    = $fields->{name}{stringValue};
    my $imgfile = $fields->{imgfile}{stringValue};

    # Price can be double or integer
    my $price = $fields->{price}{doubleValue};
    unless ( defined $price ) {
        $price = $fields->{price}{integerValue};
    }

    # Quantity is integer (string in JSON)
    my $quantity = $fields->{quantity}{integerValue};

    # Dates
    my $timestamp       = $fields->{timestamp}{timestampValue};
    my $actualdateadded = $fields->{actualdateadded}{timestampValue};

    return undef
      unless defined $name
      && defined $price
      && defined $quantity
      && defined $imgfile;

    return {
        id              => $id,
        name            => $name,
        price           => $price + 0.0,       # ensure numeric
        quantity        => $quantity + 0,
        imgfile         => $imgfile,
        timestamp       => $timestamp,
        actualdateadded => $actualdateadded,
    };
}

sub _product_to_firestore_fields ( $self, $product ) {
    return {
        name      => { stringValue    => $product->{name} },
        price     => { doubleValue    => $product->{price} + 0.0 },
        quantity  => { integerValue   => "$product->{quantity}" },   # stringify
        imgfile   => { stringValue    => $product->{imgfile} },
        timestamp => { timestampValue => $product->{timestamp} },
        actualdateadded => { timestampValue => $product->{actualdateadded} },
    };
}

1;
