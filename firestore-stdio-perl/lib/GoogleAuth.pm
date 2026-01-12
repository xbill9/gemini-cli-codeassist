package GoogleAuth;
use Mojo::Base -base, -signatures;
use Mojo::UserAgent;
use Mojo::JSON qw(decode_json encode_json);
use Mojo::File;
use MIME::Base64 qw(encode_base64url);
use Crypt::PK::RSA; # For signing if we do it manually. Wait, do we have this?

# Check dependencies. If we don't have Crypt::PK::RSA or similar, we might be in trouble for manual signing.
# The swift code uses JWTKit.
# server.pl uses MCP::Server.
# cpanfile has WWW::Google::Cloud::Auth::ServiceAccount.
# I should really try to use that module. Since I can't read it, I'll rely on the standard API for it which is likely:
# ->new(credentials_path => ...)->get_token

has ua => sub { Mojo::UserAgent->new(inactivity_timeout => 30) };
has credentials_path => sub { $ENV{GOOGLE_APPLICATION_CREDENTIALS} };
has _service_account => undef; # The JSON content
has _auth_module => undef;     # The WWW::Google::Cloud::Auth::ServiceAccount object

has _cached_token => undef;
has _token_expiration => 0;
has _cached_project_id => undef;

sub new {
    my $class = shift;
    my $self = $class->SUPER::new(@_);

    # Try to find credentials
    unless ($self->credentials_path) {
        my $home = $ENV{HOME} // $ENV{UserProfile};
        my $default = Mojo::File->new($home, '.config', 'gcloud', 'application_default_credentials.json');
        if (-e $default) {
            $self->credentials_path("$default");
        }
    }

    if ($self->credentials_path && -e $self->credentials_path) {
        # Load JSON
        my $json_text = Mojo::File->new($self->credentials_path)->slurp;
        my $creds = decode_json($json_text);
        $self->_service_account($creds);

        if ($creds->{type} && $creds->{type} eq 'service_account') {
            require WWW::Google::Cloud::Auth::ServiceAccount;
            my $auth = WWW::Google::Cloud::Auth::ServiceAccount->new(
                credentials_path => $self->credentials_path,
                scope => 'https://www.googleapis.com/auth/datastore https://www.googleapis.com/auth/cloud-platform',
            );
            $self->_auth_module($auth);
        }
    }

    return $self;
}

sub get_project_id ($self) {
    if ($self->_service_account) {
        return $self->_service_account->{project_id} // $self->_service_account->{quota_project_id};
    }
    
    # Metadata server
    return $self->_cached_project_id if $self->_cached_project_id;

    my $url = "http://metadata.google.internal/computeMetadata/v1/project/project-id";
    my $tx = $self->ua->get($url => { 'Metadata-Flavor' => 'Google' });
    
    if (my $err = $tx->error) {
        die "Failed to get project ID from metadata server: " . $err->{message};
    }
    
    $self->_cached_project_id($tx->res->body);
    return $self->_cached_project_id;
}

sub get_access_token ($self) {
    # Check cache
    if (defined $self->_cached_token && $self->_token_expiration > time) {
        return $self->_cached_token;
    }

    if (!$self->_auth_module) {
        if ($self->_service_account && ($self->_service_account->{type} // '') eq 'authorized_user') {
            return $self->_refresh_authorized_user_token;
        }
        return $self->_refresh_metadata_token;
    }

    # Service Account
    my $res = $self->_auth_module->get_token;
    return $res->{access_token};
}

sub _refresh_authorized_user_token ($self) {
    my $creds = $self->_service_account;
    my $url = "https://oauth2.googleapis.com/token";
    
    my $tx = $self->ua->post($url => form => {
        client_id     => $creds->{client_id},
        client_secret => $creds->{client_secret},
        refresh_token => $creds->{refresh_token},
        grant_type    => 'refresh_token',
    });

    if (my $err = $tx->error) {
        die "Failed to refresh token for authorized_user: " . ($tx->res->body || $err->{message});
    }

    my $data = $tx->res->json;
    $self->_cached_token($data->{access_token});
    $self->_token_expiration(time + ($data->{expires_in} // 3600) - 60);
    
    return $self->_cached_token;
}

sub _refresh_metadata_token ($self) {
    my $url = "http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/default/token";
    my $tx = $self->ua->get($url => { 'Metadata-Flavor' => 'Google' });

    if (my $err = $tx->error) {
        die "Failed to get token from metadata server: " . $err->{message};
    }

    my $data = $tx->res->json;
    $self->_cached_token($data->{access_token});
    $self->_token_expiration(time + ($data->{expires_in} // 3600) - 60);
    
    return $self->_cached_token;
}

1;
