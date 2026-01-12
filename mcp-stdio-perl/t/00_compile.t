use strict;
use warnings;
use Test::More;

# Check if the server script compiles
my $output = `PERL5LIB=local/lib/perl5 perl -c server.pl 2>&1`;
ok($? == 0, "server.pl compiles") or diag($output);

done_testing;
