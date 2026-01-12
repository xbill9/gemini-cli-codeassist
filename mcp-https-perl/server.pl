#!/usr/bin/env perl
use Mojolicious::Lite -signatures;
use MCP::Server;
use Mojo::Log;
use JSON::MaybeXS;

# Initialize JSON encoder for logging
my $json = JSON::MaybeXS->new->utf8->canonical;

# Setup Logger to mimic python-json-logger
# We will use Mojo::Log but format the output as JSON to STDERR
app->log->format(
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

my $log = app->log;

my $server = MCP::Server->new;

$server->tool(
    name         => 'greet',
    description  => 'Get a greeting from a local HTTP server.',
    input_schema => {
        type       => 'object',
        properties => {
            param => { type => 'string' }
        },
        required => ['param']
    },
    code => sub ( $tool, $args ) {

# Log debug message (note: level is info by default, so this might not show unless level is changed,
# but matching main.py which sets level to INFO, so debug logs are actually NOT shown in main.py
# unless I misread. Wait, main.py sets logger.setLevel(logging.INFO).
# But then it calls logger.debug("Executed greet tool").
# So the log will NOT be output in the Python version either!
# However, to be faithful to the code:
        $log->debug("Executed greet tool");

        return $args->{param};
    }
);

# Use HTTP transport
any '/mcp' => $server->to_action;

app->start;

