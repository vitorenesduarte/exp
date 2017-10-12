#!/usr/bin/env bash

NUM_NODES=22
NAME=lsim

if [ "$1" = "start" ]; then

  # create the cluster
  gcloud container clusters \
    create ${NAME} \
    --num-nodes ${NUM_NODES}

    # --additional-zones us-central1a,us-central1b,us-central1c,us-central1f,us-east1b,us-east1c,us-east1d,us-west1b,us-west1c,us-west1a,europe-west1b,europe-west1c,europe-west1d,europe-west2c,europe-west2b,europe-west3b,europe-west3a,europe-west3c \

elif [ "$1" = "stop" ]; then

  # delete the cluster
  yes | gcloud container clusters \
    delete ${NAME} \

fi
