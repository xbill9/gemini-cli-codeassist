# Makefile for firestore-stdio-perl

.PHONY: all install run test lint format clean

all: install test

# Target to install dependencies
install:
	@echo "Installing dependencies..."
	@cpanm --installdeps . -L local

# Target to run the application
run:
	@echo "Running the application..."
	@perl -Ilocal/lib/perl5 server.pl

release:
	@echo "Running the application..."
	@perl -Ilocal/lib/perl5 server.pl

# Target to run tests
test:
	@echo "Running tests..."
	@if [ -d "t" ]; then prove -Ilocal/lib/perl5 -Ilib -r t; else echo "No tests found in t/"; fi

# Target to lint the code
lint:
	@echo "Linting the code..."
	@PERL5LIB=local/lib/perl5 perl local/bin/perlcritic server.pl

# Target to format the code
format:
	@echo "Formatting the code..."
	@PERL5LIB=local/lib/perl5 perl local/bin/perltidy -b -bext='/' server.pl

.PHONY: clean
clean:
	@echo "Cleaning up..."
	@rm -rf local
