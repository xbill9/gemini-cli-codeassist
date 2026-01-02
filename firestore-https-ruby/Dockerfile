# Use an official Ruby runtime as a parent image.
FROM ruby:3.4-slim

# Set environment variables
ENV PORT=8080
ENV BUNDLE_DEPLOYMENT=1
ENV BUNDLE_WITHOUT="development test"

# Set the working directory in the container
WORKDIR /app

# Install system dependencies required for native extensions (like puma)
RUN apt-get update -qq && \
    apt-get install -y build-essential && \
    rm -rf /var/lib/apt/lists/*

# Copy Gemfile and Gemfile.lock
COPY Gemfile Gemfile.lock ./

# Install gems
RUN bundle install

# Copy the rest of the application code
COPY . .

# Expose the port the app runs on
EXPOSE 8080

# Run the application
CMD ["bundle", "exec", "ruby", "main.rb"]