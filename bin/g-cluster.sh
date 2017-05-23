#!/usr/bin/env bash

NUM_NODES=4
NAME=lsim

if [ "$1" = "start" ]; then

  # create the cluster
  gcloud container clusters \
    create ${NAME} \
    --num-nodes=${NUM_NODES}

elif [ "$1" = "stop" ]; then

  # delete the cluster
  yes | gcloud container clusters \
    delete ${NAME}

fi
