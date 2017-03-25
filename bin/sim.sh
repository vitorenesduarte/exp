#!/usr/bin/env bash

declare -A CONFIG
#ldb_mode;
#ldb_driven_mode;
#ldb_redundant_dgroups;
#ldb_dgroup_back_propagation
CONFIG[0]="state_based;none;false;false"
#CONFIG[1]="state_based;state_driven;false;false"
#CONFIG[2]="state_based;digest_driven;false;false"
CONFIG[3]="delta_based;none;false;false"
CONFIG[4]="delta_based;none;true;false"
CONFIG[5]="delta_based;none;false;true"
CONFIG[6]="delta_based;none;true;true"

DIR=$(dirname $0)
SCRIPT=${DIR}/lsim-deploy.sh
BRANCH=$(git branch | grep "*" | awk '{print $2}')
OVERLAY=hyparview
SIMULATION=gset
NODE_NUMBER=16
NODE_EVENT_NUMBER=50

echo "[$(date +%T)] Starting ${SIMULATION} simulation on branch ${BRANCH}"

# start redis
${DIR}/redis-deploy.sh

for i in "${CONFIG[@]}"
do
  R=(${i//;/ })
  LDB_MODE=${R[0]}
  LDB_DRIVEN_MODE=${R[1]}
  LDB_REDUNDANT_DGROUPS=${R[2]}
  LDB_DGROUP_BACK_PROPAGATION=${R[3]}

  BRANCH=${BRANCH} \
    LDB_MODE=${LDB_MODE} \
    LDB_DRIVEN_MODE=${LDB_DRIVEN_MODE} \
    LDB_REDUNDANT_DGROUPS=${LDB_REDUNDANT_DGROUPS} \
    LDB_DGROUP_BACK_PROPAGATION=${LDB_DGROUP_BACK_PROPAGATION} \
    OVERLAY=${OVERLAY} \
    SIMULATION=${SIMULATION} \
    NODE_NUMBER=${NODE_NUMBER} \
    NODE_EVENT_NUMBER=${NODE_EVENT_NUMBER} ${SCRIPT}

  MINUTES=5

  echo "[$(date +%T)] Running ${R[@]}"
  echo "[$(date +%T)] Waiting ${MINUTES} minute(s) before next deploy."
  sleep $((60 * ${MINUTES}))
done
