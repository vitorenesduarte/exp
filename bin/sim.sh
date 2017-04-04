#!/usr/bin/env bash

DIR=$(dirname "$0")
BRANCH=$(git branch |
         grep "^\*" |
         awk '{print $2}')

if [ "${WHAT}" == "build" ]; then
  IMAGE=vitorenesduarte/lsim
  PULL_IMAGE=Always
  DOCKERFILE=${DIR}/../Dockerfiles/lsim

  # build and push image
  BRANCH=${BRANCH} \
    IMAGE=${IMAGE} \
    DOCKERFILE=${DOCKERFILE} "${DIR}"/image.sh

elif [ "${WHAT}" == "run" ]; then
  IMAGE=vitorenesduarte/lsim
  PULL_IMAGE=IfNotPresent
else
  # otherwise use image that clones on start
  IMAGE=vitorenesduarte/lsim-dev
  PULL_IMAGE=IfNotPresent
fi

# start redis
"${DIR}"/redis-deploy.sh

# start lsim-dash
"${DIR}"/lsim-dash-deploy.sh

declare -A CONFIG
#ldb_mode;
#ldb_driven_mode;
#ldb_redundant_dgroups;
#ldb_dgroup_back_propagation
#CONFIG[0]="state_based;none;false;false"
CONFIG[1]="state_based;state_driven;false;false"
CONFIG[2]="state_based;digest_driven;false;false"
#CONFIG[3]="delta_based;none;false;false"
#CONFIG[4]="delta_based;none;true;false"
#CONFIG[5]="delta_based;none;false;true"
#CONFIG[6]="delta_based;none;true;true"

OVERLAY=ring
SIMULATION=gset
NODE_NUMBER=3
NODE_EVENT_NUMBER=10

echo "[$(date +%T)] Starting ${SIMULATION} simulation."
echo "[$(date +%T)] BRANCH: ${BRANCH}"
echo "[$(date +%T)] IMAGE: ${IMAGE}"

for i in "${CONFIG[@]}"
do
  R=(${i//;/ })
  LDB_MODE=${R[0]}
  LDB_DRIVEN_MODE=${R[1]}
  LDB_REDUNDANT_DGROUPS=${R[2]}
  LDB_DGROUP_BACK_PROPAGATION=${R[3]}

  BRANCH=${BRANCH} \
    IMAGE=${IMAGE} \
    PULL_IMAGE=${PULL_IMAGE} \
    LDB_MODE=${LDB_MODE} \
    LDB_DRIVEN_MODE=${LDB_DRIVEN_MODE} \
    LDB_REDUNDANT_DGROUPS=${LDB_REDUNDANT_DGROUPS} \
    LDB_DGROUP_BACK_PROPAGATION=${LDB_DGROUP_BACK_PROPAGATION} \
    OVERLAY=${OVERLAY} \
    SIMULATION=${SIMULATION} \
    NODE_NUMBER=${NODE_NUMBER} \
    NODE_EVENT_NUMBER=${NODE_EVENT_NUMBER} "${DIR}"/lsim-deploy.sh

  SECONDS=0

  echo "[$(date +%T)] Running ${R[*]}"
  echo "[$(date +%T)] Waiting ${SECONDS} second(s) before next deploy."
  sleep ${SECONDS}
done
