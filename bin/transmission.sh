#!/usr/bin/env bash

declare -A CONFIG
#ldb_mode;ldb_join_decompositions
CONFIG[0]="state_based;false"
CONFIG[1]="delta_based;false"
CONFIG[2]="delta_based;true"

DIR=$(dirname $0)
SCRIPT=${DIR}/lsim-deploy.sh
BRANCH=$(git branch | grep "*" | awk '{print $2}')
OVERLAY=ring
SIMULATION=gset
NODE_NUMBER=3
NODE_EVENT_NUMBER=50

# start redis
${DIR}/redis-deploy.sh

for i in "${CONFIG[@]}"
do
  R=(${i//;/ })
  LDB_MODE=${R[0]}
  LDB_JOIN_DECOMPOSITIONS=${R[1]}

  BRANCH=${BRANCH} \
    LDB_MODE=${LDB_MODE} \
    LDB_JOIN_DECOMPOSITIONS=${LDB_JOIN_DECOMPOSITIONS} \
    OVERLAY=${OVERLAY} \
    SIMULATION=${SIMULATION} \
    NODE_NUMBER=${NODE_NUMBER} \
    NODE_EVENT_NUMBER=${NODE_EVENT_NUMBER} ${SCRIPT}
done
