#!/usr/bin/env bash

set -e

gcloud beta emulators pubsub start \
  --host-port=0.0.0.0:8085 \
  --project="$PROJECT_ID"
