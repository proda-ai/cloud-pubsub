FROM google/cloud-sdk:343.0.0-emulators
RUN gcloud components install pubsub-emulator && \
    gcloud components update
WORKDIR /app
COPY ./start.sh /app/start.sh
CMD [ "/app/start.sh" ]
