# Use an official Perl runtime as a parent image.
FROM perl:5.36

# Set environment variables
ENV PORT=8080

# Set the working directory in the container
WORKDIR /app

# Install cpanm if not already present (it usually is in official perl images)
RUN cpanm -n App::cpanminus

# Copy cpanfile to install dependencies
COPY cpanfile .

# Install dependencies
RUN cpanm --installdeps .

# Copy the application code
COPY server.pl .

# Expose the port the app runs on
EXPOSE 8080

# Run the application using Mojolicious daemon
CMD ["sh", "-c", "perl server.pl daemon -l http://0.0.0.0:${PORT}"]

