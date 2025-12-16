# Use an official Python runtime as a parent image.
# using slim for smaller image size
FROM python:3.13-slim

# Set environment variables
# PYTHONUNBUFFERED=1 ensures that Python output is sent straight to terminal (container log) without being buffered
ENV PYTHONUNBUFFERED=1
ENV PORT=8080

# Set the working directory in the container
WORKDIR /app

# Install any needed packages specified in requirements.txt
COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

# Copy the current directory contents into the container
COPY main.py .

# Create a non-root user and switch to it
# It is a security best practice to run containers as non-root
RUN adduser --disabled-password --gecos "" appuser && chown -R appuser /app
USER appuser

# Expose the port the app runs on
EXPOSE 8080

# Run the application using uvicorn
CMD ["python", "main.py"]
