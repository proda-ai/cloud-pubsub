FROM google/cloud-sdk:360.0.0-emulators
RUN gcloud components install pubsub-emulator
WORKDIR /app
COPY ./start.sh /app/start.sh
CMD [ "/app/start.sh" ]
