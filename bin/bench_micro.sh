#!/usr/bin/env bash

REPS=1
DIR=$(dirname "$0")
PULL_IMAGE=Always

# start redis
"${DIR}"/redis-deploy.sh

# ensures each node only
# has one pod running (if nodes have 8 CPU)
CPU=7

# event number, event interval, state sync interval
SPEED_CONFIG_=(
    "100 1000 1000"
)

# overlay nodes
OVERLAY_CONFIG_=(
    "partialmesh 15"
    "tree        15"
)

# exp configuration retwis_zipf
SIM_CONFIG_=(
    "gset     0   0"
    "gcounter 0   0"
    "gmap     10  0"
    "gmap     30  0"
    "gmap     60  0"
    "gmap     100 0"
)

# ldb configuration
# mode bp rr
LDB_=(
    "state_based undefined undefined"
    "vanilla_scuttlebutt undefined undefined"
    "scuttlebutt undefined undefined"
    "delta_based false     false"
    "delta_based true      false"
    "delta_based false     true"
    "delta_based true      true"
)

# number of experiments
NEXP=$((${#OVERLAY_CONFIG_[@]} * ${#SIM_CONFIG_[@]} * ${#SPEED_CONFIG_[@]} * ${#LDB_[@]}))
EXP=1

echo "Found ${NEXP} configurations. Let's start!"

# shellcheck disable=SC2034
for REP in $(seq 1 $REPS); do
    for SPEED_CONFIG in "${SPEED_CONFIG_[@]}"; do
        SPEED_CONFIG=($(echo ${SPEED_CONFIG} | tr ' ' '\n'))
        NODE_EVENT_NUMBER=${SPEED_CONFIG[0]}
        EVENT_INTERVAL=${SPEED_CONFIG[1]}
        LDB_STATE_SYNC_INTERVAL=${SPEED_CONFIG[2]}

        for OVERLAY_CONFIG in "${OVERLAY_CONFIG_[@]}"; do
            OVERLAY_CONFIG=($(echo ${OVERLAY_CONFIG} | tr ' ' '\n'))
            OVERLAY=${OVERLAY_CONFIG[0]}
            NODE_NUMBER=${OVERLAY_CONFIG[1]}

            for SIM_CONFIG in "${SIM_CONFIG_[@]}"; do
                SIM_CONFIG=($(echo ${SIM_CONFIG} | tr ' ' '\n'))
                SIMULATION=${SIM_CONFIG[0]}
                GMAP_SIMULATION_KEY_PERCENTAGE=${SIM_CONFIG[1]}
                RETWIS_ZIPF=${SIM_CONFIG[2]}

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
                        RETWIS_ZIPF=${RETWIS_ZIPF} \
                        NODE_NUMBER=${NODE_NUMBER} \
                        NODE_EVENT_NUMBER=${NODE_EVENT_NUMBER} \
                        EVENT_INTERVAL=${EVENT_INTERVAL} \
                        CPU=${CPU} "${DIR}"/deploy-exp.sh

                    echo "[$(date +%T)] ${EXP} of ${NEXP} ended!"
                    EXP=$((EXP + 1))
                done
            done
        done
    done
done
