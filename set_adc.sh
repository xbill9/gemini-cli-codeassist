#!/bin/bash

# This script sets various Google Cloud related environment variables.
# It must be SOURCED to make the variables available in your current shell.
# Example: source ./set_env.sh

gcloud auth application-default login
