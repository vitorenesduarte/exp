#!/usr/bin/env bash

DIR=$(dirname "$0")

ENV_VARS=(
  IMAGE
  PULL_IMAGE
  LDB_MODE
  LDB_STATE_SYNC_INTERVAL
  LDB_REDUNDANT_DGROUPS
  LDB_DGROUP_BACK_PROPAGATION
  OVERLAY
  SIMULATION
  GMAP_SIMULATION_KEY_PERCENTAGE
  NODE_NUMBER
  NODE_EVENT_NUMBER
  BREAK_LINKS
  CPU
)

for ENV_VAR in "${ENV_VARS[@]}"
do
  if [ -z "${!ENV_VAR}" ]; then
    echo ">>> ${ENV_VAR} is not configured; please export it."
    exit 1
  fi
done

echo "[$(date +%T)] Configuration: "
echo "    IMAGE: ${IMAGE}"
echo "    PULL_IMAGE: ${PULL_IMAGE}"
echo "    LDB_MODE: ${LDB_MODE}"
echo "    LDB_STATE_SYNC_INTERVAL: ${LDB_STATE_SYNC_INTERVAL}"
echo "    LDB_REDUNDANT_DGROUPS: ${LDB_REDUNDANT_DGROUPS}"
echo "    LDB_DGROUP_BACK_PROPAGATION: ${LDB_DGROUP_BACK_PROPAGATION}"
echo "    OVERLAY: ${OVERLAY}"
echo "    SIMULATION: ${SIMULATION}"
echo "    GMAP_SIMULATION_KEY_PERCENTAGE: ${GMAP_SIMULATION_KEY_PERCENTAGE}"
echo "    NODE_NUMBER: ${NODE_NUMBER}"
echo "    NODE_EVENT_NUMBER: ${NODE_EVENT_NUMBER}"
echo "    BREAK_LINKS: ${BREAK_LINKS}"
echo "    CPU: ${CPU}"

# ENV SETUP:
# Kubernetes server and auth token
APISERVER=$(bin/k8s_api_server.sh)
TOKEN=$(bin/k8s_api_token.sh)

ORCHESTRATION=kubernetes
METRICS_STORE=redis

# Evaluation timestamp: unix timestamp + random
R=$(echo $RANDOM + 10000 | bc)
TIMESTAMP=$(date +%s)${R}

# Port
PEER_PORT=6866

# DEPLOYMENT:
# Deployment names
RSG_NAME=rsg-${TIMESTAMP}
EXP_NAME=exp-${TIMESTAMP}

# YAML file
FILE=/tmp/${TIMESTAMP}.yaml

cat <<EOF > "${FILE}"
apiVersion: extensions/v1beta1
kind: Deployment
metadata:
  name: "${RSG_NAME}"
spec:
  replicas: 1
  template:
    metadata:
      labels:
        timestamp: "${TIMESTAMP}"
        tag: rsg
    spec:
      containers:
      - name: "${RSG_NAME}"
        image: "${IMAGE}"
        imagePullPolicy: "${PULL_IMAGE}"
        env:
        - name: ORCHESTRATION
          value: "${ORCHESTRATION}"
        - name: METRICS_STORE
          value: "${METRICS_STORE}"
        - name: IP
          valueFrom:
            fieldRef:
              fieldPath: status.podIP
        - name: APISERVER
          value: "${APISERVER}"
        - name: TOKEN
          value: "${TOKEN}"
        - name: TIMESTAMP
          value: "${TIMESTAMP}"
        - name: LDB_MODE
          value: "${LDB_MODE}"
        - name: LDB_STATE_SYNC_INTERVAL
          value: "${LDB_STATE_SYNC_INTERVAL}"
        - name: LDB_REDUNDANT_DGROUPS
          value: "${LDB_REDUNDANT_DGROUPS}"
        - name: LDB_DGROUP_BACK_PROPAGATION
          value: "${LDB_DGROUP_BACK_PROPAGATION}"
        - name: OVERLAY
          value: "${OVERLAY}"
        - name: SIMULATION
          value: "${SIMULATION}"
        - name: GMAP_SIMULATION_KEY_PERCENTAGE
          value: "${GMAP_SIMULATION_KEY_PERCENTAGE}"
        - name: NODE_NUMBER
          value: "${NODE_NUMBER}"
        - name: NODE_EVENT_NUMBER
          value: "${NODE_EVENT_NUMBER}"
        - name: BREAK_LINKS
          value: "${BREAK_LINKS}"
        - name: RSG
          value: "true"
---
apiVersion: extensions/v1beta1
kind: Deployment
metadata:
  name: "${EXP_NAME}"
spec:
  replicas: ${NODE_NUMBER}
  template:
    metadata:
      labels:
        timestamp: "${TIMESTAMP}"
        tag: exp
    spec:
      containers:
      - name: "${EXP_NAME}"
        image: "${IMAGE}"
        imagePullPolicy: "${PULL_IMAGE}"
        resources:
          requests:
            cpu: "${CPU}"
        securityContext:
          privileged: true
        env:
        - name: ORCHESTRATION
          value: "${ORCHESTRATION}"
        - name: METRICS_STORE
          value: "${METRICS_STORE}"
        - name: IP
          valueFrom:
            fieldRef:
              fieldPath: status.podIP
        - name: PEER_PORT
          value: "${PEER_PORT}"
        - name: APISERVER
          value: "${APISERVER}"
        - name: TOKEN
          value: "${TOKEN}"
        - name: TIMESTAMP
          value: "${TIMESTAMP}"
        - name: LDB_MODE
          value: "${LDB_MODE}"
        - name: LDB_STATE_SYNC_INTERVAL
          value: "${LDB_STATE_SYNC_INTERVAL}"
        - name: LDB_REDUNDANT_DGROUPS
          value: "${LDB_REDUNDANT_DGROUPS}"
        - name: LDB_DGROUP_BACK_PROPAGATION
          value: "${LDB_DGROUP_BACK_PROPAGATION}"
        - name: LDB_METRICS
          value: "true"
        - name: OVERLAY
          value: "${OVERLAY}"
        - name: SIMULATION
          value: "${SIMULATION}"
        - name: GMAP_SIMULATION_KEY_PERCENTAGE
          value: "${GMAP_SIMULATION_KEY_PERCENTAGE}"
        - name: NODE_NUMBER
          value: "${NODE_NUMBER}"
        - name: NODE_EVENT_NUMBER
          value: "${NODE_EVENT_NUMBER}"
        - name: BREAK_LINKS
          value: "${BREAK_LINKS}"
        - name: RSG
          value: "false"
EOF

kubectl create -f "${FILE}"

# wait until the end of the experiment
while [ $(kubectl get pods -l timestamp=${TIMESTAMP} 2> /dev/null | wc -l) -gt 0 ]; do
  sleep 3
done

# fetch logs from redis
${DIR}/start-redis-sync.sh

echo "[$(date +%T)] Done!"
