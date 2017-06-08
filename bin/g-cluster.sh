#!/usr/bin/env bash

NUM_NODES=3
NAME=lsim
MACHINE=n1-standard-8

if [ "$1" = "start" ]; then

  # create the cluster
  gcloud container clusters \
    create ${NAME} \
    --num-nodes=${NUM_NODES} \
    --machine-type=${MACHINE}

elif [ "$1" = "stop" ]; then

  # delete the cluster
  yes | gcloud container clusters \
    delete ${NAME}

fi
