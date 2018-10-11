#!/usr/bin/env bash

DIR=$(dirname "$0")

NUM_NODES=3
ZONE=europe-north1-b
NAME=exp
MACHINE=n1-standard-8
MACHINE=n1-standard-1
VERSION=1.9.7-gke.6

if [ "$1" = "start" ]; then

    # create the cluster
    gcloud container clusters \
        create ${NAME} \
        --zone=${ZONE} \
        --num-nodes=${NUM_NODES} \
        --machine-type=${MACHINE} \
        --cluster-version ${VERSION} \
        --preemptible

elif [ "$1" = "stop" ]; then

    # delete the cluster
    yes | gcloud container clusters \
        delete ${NAME}

fi
