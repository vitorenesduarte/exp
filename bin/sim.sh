#!/usr/bin/env bash

declare -A CONFIG
#ldb_mode;ldb_redundant_dgroups;ldb_dgroup_back_propagation
CONFIG[0]="state_based;false;false"
CONFIG[1]="delta_based;false;false"
CONFIG[2]="delta_based;true;false"
CONFIG[3]="delta_based;false;true"
CONFIG[4]="delta_based;true;true"

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
  LDB_REDUNDANT_DGROUPS=${R[1]}
  LDB_DGROUP_BACK_PROPAGATION=${R[2]}

  BRANCH=${BRANCH} \
    LDB_MODE=${LDB_MODE} \
    LDB_REDUNDANT_DGROUPS=${LDB_REDUNDANT_DGROUPS} \
    LDB_DGROUP_BACK_PROPAGATION=${LDB_DGROUP_BACK_PROPAGATION} \
    OVERLAY=${OVERLAY} \
    SIMULATION=${SIMULATION} \
    NODE_NUMBER=${NODE_NUMBER} \
    NODE_EVENT_NUMBER=${NODE_EVENT_NUMBER} ${SCRIPT}
done
