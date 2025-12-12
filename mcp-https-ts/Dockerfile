# Copyright 2023 Google LLC
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# Use a Node.js LTS (Long Term Support) image as the base for building
FROM node:24-slim AS builder

WORKDIR /usr/app

# Copy package.json and package-lock.json to leverage Docker cache for npm install
COPY package*.json ./
RUN npm install

# Copy the rest of the application source code
COPY . .

# Build the project
RUN npm run build

# Use a minimal image for the final runtime stage
FROM gcr.io/distroless/nodejs24-debian12

# Set the working directory
WORKDIR /usr/app

# Copy only the necessary files from the builder stage
COPY --from=builder /usr/app/dist ./dist
COPY --from=builder /usr/app/node_modules ./node_modules
COPY --from=builder /usr/app/package*.json ./

# Expose the port the app runs on (defaulting to 8080 as per Cloud Run convention)
ENV PORT 8080
EXPOSE 8080

# Run as a non-root user for security
# The distroless nodejs image already sets up a non-root user 'nodejs' (UID 65532)
# USER nodejs

# Run the web service on container startup.
CMD ["dist/index.js"]
