#!/usr/bin/env bash

REPS=1
DIR=$(dirname "$0")
BRANCH=$(git branch |
         grep "^\*" |
         awk '{print $2}')

# "${DIR}"/g-cluster.sh start

if [ "$1" == "build" ]; then
  # build, push and use that image
  IMAGE=vitorenesduarte/lsim
  PULL_IMAGE=Always
  DOCKERFILE=${DIR}/../Dockerfiles/lsim

  BRANCH=${BRANCH} \
    IMAGE=${IMAGE} \
    DOCKERFILE=${DOCKERFILE} "${DIR}"/image.sh

elif [ "$1" == "clone" ]; then
  # use image that clones on start
  IMAGE=vitorenesduarte/lsim-dev
  PULL_IMAGE=IfNotPresent

elif [ "$1" == "clone-gry" ]; then
  # build, push and use that image
  IMAGE=gyounes/lsim-gry
  PULL_IMAGE=Always
  DOCKERFILE=${DIR}/../Dockerfiles/lsim-gry

  BRANCH=${BRANCH} \
    IMAGE=${IMAGE} \
    DOCKERFILE=${DOCKERFILE} "${DIR}"/image.sh

elif [ "$1" == "build-gry" ]; then
  # build, push and use that image
  IMAGE=gyounes/lsim-gyounes
  PULL_IMAGE=Always
  DOCKERFILE=${DIR}/../Dockerfiles/lsim-gyounes

    DIR=${DIR} \
    IMAGE=${IMAGE} \
    DOCKERFILE=${DOCKERFILE} "${DIR}"/image-gyounes.sh

else
  # use the latest lsim image
  IMAGE=vitorenesduarte/lsim
  PULL_IMAGE=IfNotPresent

fi

# start redis
"${DIR}"/redis-deploy.sh

# start dashboard
#"${DIR}"/lsim-dash-deploy.sh

# lsim configuration
SIMULATION_=(trcb_Dots)
OVERLAY=trcb
NODE_NUMBER_=(5)
NODE_EVENT_NUMBER_=(100)
PARTITION_NUMBER=1
ELEMENT_NODE_RATIO=1
KEEP_ALIVE=false

# shellcheck disable=SC2034
for REP in $(seq 1 $REPS)
do
  for SIMULATION in "${SIMULATION_[@]}"
  do
    for NODE_NUMBER in "${NODE_NUMBER_[@]}"
    do
      for NODE_EVENT_NUMBER in "${NODE_EVENT_NUMBER_[@]}"
      do
        BRANCH=${BRANCH} \
        IMAGE=${IMAGE} \
        PULL_IMAGE=${PULL_IMAGE} \
        SIMULATION=${SIMULATION} \
        OVERLAY=${OVERLAY} \
        PARTITION_NUMBER=${PARTITION_NUMBER} \
        NODE_NUMBER=${NODE_NUMBER} \
        NODE_EVENT_NUMBER=${NODE_EVENT_NUMBER} \
        ELEMENT_NODE_RATIO=${ELEMENT_NODE_RATIO} \
        KEEP_ALIVE=${KEEP_ALIVE} "${DIR}"/lsim-deploy.sh
      done
    done
  done
done

#"${DIR}"/start-redis-sync.sh

#"${DIR}"/g-cluster.sh stop
