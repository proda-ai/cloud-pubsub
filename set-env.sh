#!/usr/bin/env bash

set -o allexport

if [[ -z "$PROJECT_ID" ]]; then
    PROJECT_ID="local-project"
fi

if [[ -z "$PUBSUB_EMULATOR_HOST" ]] && [[ -z "$GOOGLE_APPLICATION_CREDENTIALS" ]]; then
    PUBSUB_EMULATOR_HOST="localhost:8085"
fi

# To run tests against hosted Cloud PubSub:
# Option 1 (recommended for local): Use gcloud auth application-default login
#   This creates credentials at ~/.config/gcloud/application_default_credentials.json
#   No need to set GOOGLE_APPLICATION_CREDENTIALS
# Option 2 (legacy): Set GOOGLE_APPLICATION_CREDENTIALS to a service account key file
#   GOOGLE_APPLICATION_CREDENTIALS="secrets/service_account.json"
# Option 3 (GCP VM/CI): Don't set GOOGLE_APPLICATION_CREDENTIALS, use metadata server

set +o allexport
