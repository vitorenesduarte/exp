#!/usr/bin/env bash

REPS=1
DIR=$(dirname "$0")
DOCKER_USER=vitorenesduarte
IMAGE=${DOCKER_USER}/lsim-copy
DOCKERFILE=${DIR}/../Dockerfiles/lsim-copy

# always pull image,
# unless local
PULL_IMAGE=Always

if [ "$1" == "build" ]; then
  # build and push
  IMAGE=${IMAGE} \
    DOCKERFILE=${DOCKERFILE} "${DIR}"/image.sh

elif [ "$1" == "local" ]; then
  # build locally
  eval $(minikube docker-env)
  docker build \
         --no-cache \
         -t "${IMAGE}" -f "${DOCKERFILE}" .

  # use the new image
  PULL_IMAGE=Never
fi

# start redis
"${DIR}"/redis-deploy.sh

# start dashboard
#"${DIR}"/lsim-dash-deploy.sh

CPU=7

# overlay nodes
OVERLAY_CONFIG_=(
   "chord 16"
   "tree 14"
)

# lsim configuration
SIM_CONFIG_=(
  "gset 0"
  "gmap 10"
  "gmap 30"
)
NODE_EVENT_NUMBER=200

# ldb configuration
LDB_STATE_SYNC_INTERVAL=1000
# mode driven_mode bp rr break_links
LDB_=(
   "delta_based state_driven  true      true      one"
   "delta_based none          true      true      one"
   # "delta_based none          true      true      none"
   # "delta_based none          true      false     none"
   # "delta_based none          false     true      none"
   # "delta_based none          false     false     none"
   # "state_based none          undefined undefined none"
)

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
        LDB_DRIVEN_MODE=${LDB[1]}
        LDB_DGROUP_BACK_PROPAGATION=${LDB[2]}
        LDB_REDUNDANT_DGROUPS=${LDB[3]}
        BREAK_LINKS=${LDB[4]}

        if [[ "$LDB_DRIVEN_MODE" = digest_driven ]] && [[ "$SIMULATION" = gset || "$SIMULATION" = gcounter ]]; then
          echo "Skipping..."
        else

          BRANCH=${BRANCH} \
            IMAGE=${IMAGE} \
            PULL_IMAGE=${PULL_IMAGE} \
            LDB_MODE=${LDB_MODE} \
            LDB_DRIVEN_MODE=${LDB_DRIVEN_MODE} \
            LDB_STATE_SYNC_INTERVAL=${LDB_STATE_SYNC_INTERVAL} \
            LDB_DGROUP_BACK_PROPAGATION=${LDB_DGROUP_BACK_PROPAGATION} \
            LDB_REDUNDANT_DGROUPS=${LDB_REDUNDANT_DGROUPS} \
            OVERLAY=${OVERLAY} \
            SIMULATION=${SIMULATION} \
            GMAP_SIMULATION_KEY_PERCENTAGE=${GMAP_SIMULATION_KEY_PERCENTAGE} \
            NODE_NUMBER=${NODE_NUMBER} \
            NODE_EVENT_NUMBER=${NODE_EVENT_NUMBER} \
            BREAK_LINKS=${BREAK_LINKS} \
            CPU=${CPU} "${DIR}"/lsim-deploy.sh

          # fetch logs from redis
          ${DIR}/start-redis-sync.sh
        fi
      done
    done
  done
done
