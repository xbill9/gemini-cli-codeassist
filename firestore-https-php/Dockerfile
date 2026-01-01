# Use official PHP runtime
FROM php:8.2-cli

# Set environment variables
ENV PORT=8080

# Install system dependencies
RUN apt-get update && apt-get install -y \
    git \
    zip \
    unzip \
    zlib1g-dev \
    g++ \
    make \
    autoconf

# Install gRPC and protobuf extensions
RUN export MAKEFLAGS="-j$(nproc)" && pecl install grpc && pecl install protobuf && docker-php-ext-enable grpc protobuf

# Install Composer
COPY --from=composer:latest /usr/bin/composer /usr/bin/composer

# Set working directory
WORKDIR /app

# Copy application files
COPY . .

# Install PHP dependencies
RUN composer install --no-dev --optimize-autoloader --ignore-platform-reqs

# Create sessions directory and set permissions
RUN mkdir -p sessions && chmod 777 sessions

# Expose the port
EXPOSE 8080

# Run the application
CMD ["sh", "-c", "php -S 0.0.0.0:${PORT} main.php"]
