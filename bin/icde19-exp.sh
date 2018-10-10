#!/usr/bin/env bash

REPS=1
DIR=$(dirname "$0")
DOCKER_USER=vitorenesduarte
IMAGE=${DOCKER_USER}/exp-copy:nobl
PULL_IMAGE=Always

# start redis
"${DIR}"/redis-deploy.sh

# ensures each node only
# has one pod running (if nodes have 8 CPU)
CPU=7

# overlay nodes
OVERLAY_CONFIG_=(
   "partialmesh 15"
   "tree        15"
)

# exp configuration
SIM_CONFIG_=(
  "gset     0"
  "gcounter 0"
  "gmap     10"
  "gmap     30"
  "gmap     60"
  "gmap     100"
  "retwis 0"
)
NODE_EVENT_NUMBER=100

# ldb configuration
LDB_STATE_SYNC_INTERVAL=1000
# mode bp rr
LDB_=(
   "state_based undefined undefined"
   "scuttlebutt undefined undefined"
   "delta_based false     false"
   "delta_based true      false"
   "delta_based false     true"
   "delta_based true      true"
)

# number of experiments
NEXP=$((${#OVERLAY_CONFIG_[@]} * ${#SIM_CONFIG_[@]} * ${#LDB_[@]}))
EXP=1

# shellcheck disable=SC2034
for REP in $(seq 1 $REPS); do
  for OVERLAY_CONFIG in "${OVERLAY_CONFIG_[@]}"; do
    OVERLAY_CONFIG=($(echo ${OVERLAY_CONFIG} | tr ' ' '\n'))
    OVERLAY=${OVERLAY_CONFIG[0]}
    NODE_NUMBER=${OVERLAY_CONFIG[1]}

    for SIM_CONFIG in "${SIM_CONFIG_[@]}"; do
      SIM_CONFIG=($(echo ${SIM_CONFIG} | tr ' ' '\n'))
      SIMULATION=${SIM_CONFIG[0]}
      GMAP_SIMULATION_KEY_PERCENTAGE=${SIM_CONFIG[1]}

      for LDB in "${LDB_[@]}"; do
        LDB=($(echo ${LDB} | tr ' ' '\n'))
        LDB_MODE=${LDB[0]}
        LDB_DGROUP_BACK_PROPAGATION=${LDB[1]}
        LDB_REDUNDANT_DGROUPS=${LDB[2]}

        IMAGE=${IMAGE} \
            PULL_IMAGE=${PULL_IMAGE} \
            LDB_MODE=${LDB_MODE} \
            LDB_STATE_SYNC_INTERVAL=${LDB_STATE_SYNC_INTERVAL} \
            LDB_DGROUP_BACK_PROPAGATION=${LDB_DGROUP_BACK_PROPAGATION} \
            LDB_REDUNDANT_DGROUPS=${LDB_REDUNDANT_DGROUPS} \
            OVERLAY=${OVERLAY} \
            SIMULATION=${SIMULATION} \
            GMAP_SIMULATION_KEY_PERCENTAGE=${GMAP_SIMULATION_KEY_PERCENTAGE} \
            NODE_NUMBER=${NODE_NUMBER} \
            NODE_EVENT_NUMBER=${NODE_EVENT_NUMBER} \
            CPU=${CPU} "${DIR}"/deploy-exp.sh

        echo "[$(date +%T)] ${EXP} of ${NEXP} ended!"
        EXP=$((EXP + 1))
      done
    done
  done
done
