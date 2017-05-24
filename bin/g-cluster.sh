#!/usr/bin/env bash

NUM_NODES=8
NAME=lsim
KUBE_VERSION=1.6.4

if [ "$1" = "start" ]; then

  # create the cluster
  gcloud container clusters \
    create ${NAME} \
    --num-nodes=${NUM_NODES} \
    --cluster-version=${KUBE_VERSION}

elif [ "$1" = "stop" ]; then

  # delete the cluster
  yes | gcloud container clusters \
    delete ${NAME}

fi
