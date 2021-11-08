#!/usr/bin/env bash

set -o allexport

if [[ -z "$PROJECT_ID" ]]; then
    PROJECT_ID="local-project"
fi

if [[ -z "$PUBSUB_EMULATOR_HOST" ]] && [[ -z "$GOOGLE_APPLICATION_CREDENTIALS" ]]; then
    PUBSUB_EMULATOR_HOST="localhost:8085"
fi

# To run tests against hosted Cloud PubSub set GOOGLE_APPLICATION_CREDENTIALS
# instead of PUBSUB_EMULATOR_HOST
# GOOGLE_APPLICATION_CREDENTIALS="secrets/service_account.json"

set +o allexport
