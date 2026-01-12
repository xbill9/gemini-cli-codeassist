FROM perl:5.36

WORKDIR /app

# Install system dependencies if any (none for now)

# Copy cpanfile and install dependencies
COPY cpanfile .
RUN cpanm --installdeps . -L local

# Copy application source
COPY . .

# Set entrypoint
ENTRYPOINT ["perl", "-Ilocal/lib/perl5", "server.pl"]
