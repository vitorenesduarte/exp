#!/usr/bin/env bash

REPS=1
DIR=$(dirname "$0")
DOCKER_USER=vitorenesduarte

#"${DIR}"/g-cluster.sh start

if [ "$1" == "build" ]; then
  # build and push
  IMAGE=${DOCKER_USER}/lsim-copy
  DOCKERFILE=${DIR}/../Dockerfiles/lsim-copy

  IMAGE=${IMAGE} \
    DOCKERFILE=${DOCKERFILE} "${DIR}"/image.sh

  # use the new image
  PULL_IMAGE=Always

else
  # use the latest image
  PULL_IMAGE=IfNotPresent

fi

# start redis
"${DIR}"/redis-deploy.sh

# start dashboard
#"${DIR}"/lsim-dash-deploy.sh

# lsim configuration
OVERLAY_=(ring)
SIMULATION_=(awset)
NODE_NUMBER_=(3)
NODE_EVENT_NUMBER_=(20)
PARTITION_NUMBER_=(1)
ELEMENT_NODE_RATIO=1
KEEP_ALIVE=false

# ldb configuration
MODE_=(state_based delta_based)
DRIVEN_MODE_=(none)
STATE_SYNC_INTERVAL_=(1000)
REDUNDANT_DGROUPS_=(true)
DGROUP_BACK_PROPAGATION_=(true)

# shellcheck disable=SC2034
for REP in $(seq 1 $REPS)
do
  for OVERLAY in "${OVERLAY_[@]}"
  do
    for SIMULATION in "${SIMULATION_[@]}"
    do
      for NODE_NUMBER in "${NODE_NUMBER_[@]}"
      do
        for NODE_EVENT_NUMBER in "${NODE_EVENT_NUMBER_[@]}"
        do
          for LDB_MODE in "${MODE_[@]}"
          do
            for LDB_STATE_SYNC_INTERVAL in "${STATE_SYNC_INTERVAL_[@]}"
            do
              if [ "$LDB_MODE" = state_based ]; then

                for LDB_DRIVEN_MODE in "${DRIVEN_MODE_[@]}"
                do

                  IMAGE=${IMAGE} \
                    PULL_IMAGE=${PULL_IMAGE} \
                    LDB_MODE=${LDB_MODE} \
                    LDB_DRIVEN_MODE=${LDB_DRIVEN_MODE} \
                    LDB_STATE_SYNC_INTERVAL=${LDB_STATE_SYNC_INTERVAL} \
                    LDB_REDUNDANT_DGROUPS=undefined \
                    LDB_DGROUP_BACK_PROPAGATION=undefined \
                    OVERLAY=${OVERLAY} \
                    SIMULATION=${SIMULATION} \
                    NODE_NUMBER=${NODE_NUMBER} \
                    NODE_EVENT_NUMBER=${NODE_EVENT_NUMBER} \
                    ELEMENT_NODE_RATIO=${ELEMENT_NODE_RATIO} \
                    PARTITION_NUMBER=1 \
                    KEEP_ALIVE=${KEEP_ALIVE} "${DIR}"/lsim-deploy.sh
                done

              elif [ "$LDB_MODE" = delta_based ]; then
                for LDB_REDUNDANT_DGROUPS in "${REDUNDANT_DGROUPS_[@]}"
                do
                  for LDB_DGROUP_BACK_PROPAGATION in "${DGROUP_BACK_PROPAGATION_[@]}"
                  do
                    for PARTITION_NUMBER in "${PARTITION_NUMBER_[@]}"
                    do

                      if [ "$PARTITION_NUMBER" -gt "1" ]; then

                        for LDB_DRIVEN_MODE in "${DRIVEN_MODE_[@]}"
                        do
                          IMAGE=${IMAGE} \
                            PULL_IMAGE=${PULL_IMAGE} \
                            LDB_MODE=${LDB_MODE} \
                            LDB_DRIVEN_MODE=${LDB_DRIVEN_MODE} \
                            LDB_STATE_SYNC_INTERVAL=${LDB_STATE_SYNC_INTERVAL} \
                            LDB_REDUNDANT_DGROUPS=${LDB_REDUNDANT_DGROUPS} \
                            LDB_DGROUP_BACK_PROPAGATION=${LDB_DGROUP_BACK_PROPAGATION} \
                            OVERLAY=${OVERLAY} \
                            SIMULATION=${SIMULATION} \
                            NODE_NUMBER=${NODE_NUMBER} \
                            NODE_EVENT_NUMBER=${NODE_EVENT_NUMBER} \
                            ELEMENT_NODE_RATIO=${ELEMENT_NODE_RATIO} \
                            PARTITION_NUMBER=${PARTITION_NUMBER} \
                            KEEP_ALIVE=${KEEP_ALIVE} "${DIR}"/lsim-deploy.sh

                        done
                      else

                        IMAGE=${IMAGE} \
                          PULL_IMAGE=${PULL_IMAGE} \
                          LDB_MODE=${LDB_MODE} \
                          LDB_DRIVEN_MODE=none \
                          LDB_STATE_SYNC_INTERVAL=${LDB_STATE_SYNC_INTERVAL} \
                          LDB_REDUNDANT_DGROUPS=${LDB_REDUNDANT_DGROUPS} \
                          LDB_DGROUP_BACK_PROPAGATION=${LDB_DGROUP_BACK_PROPAGATION} \
                          OVERLAY=${OVERLAY} \
                          SIMULATION=${SIMULATION} \
                          NODE_NUMBER=${NODE_NUMBER} \
                          NODE_EVENT_NUMBER=${NODE_EVENT_NUMBER} \
                          ELEMENT_NODE_RATIO=${ELEMENT_NODE_RATIO} \
                          PARTITION_NUMBER=${PARTITION_NUMBER} \
                          KEEP_ALIVE=${KEEP_ALIVE} "${DIR}"/lsim-deploy.sh

                      fi
                    done
                  done
                done
              fi
            done
          done
        done
      done
    done
  done
done

#"${DIR}"/start-redis-sync.sh

#"${DIR}"/g-cluster.sh stop
