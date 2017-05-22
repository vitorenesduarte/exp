#!/usr/bin/env bash

ENV_VARS=(
  BRANCH
  IMAGE
  PULL_IMAGE
  LDB_MODE
  LDB_DRIVEN_MODE
  LDB_STATE_SYNC_INTERVAL
  LDB_EVICTION_ROUND_NUMBER
  LDB_REDUNDANT_DGROUPS
  LDB_DGROUP_BACK_PROPAGATION
  OVERLAY
  SIMULATION
  NODE_NUMBER
  NODE_EVENT_NUMBER
  PARTITION_NUMBER
  KEEP_ALIVE
)

for ENV_VAR in "${ENV_VARS[@]}"
do
  if [ -z "${!ENV_VAR}" ]; then
    echo ">>> ${ENV_VAR} is not configured; please export it."
    exit 1
  fi
done

echo "[$(date +%T)] Configuration: "
echo "    BRANCH: ${BRANCH}"
echo "    IMAGE: ${IMAGE}"
echo "    PULL_IMAGE: ${PULL_IMAGE}"
echo "    LDB_MODE: ${LDB_MODE}"
echo "    LDB_DRIVEN_MODE: ${LDB_DRIVEN_MODE}"
echo "    LDB_STATE_SYNC_INTERVAL: ${LDB_STATE_SYNC_INTERVAL}"
echo "    LDB_EVICTION_ROUND_NUMBER: ${LDB_EVICTION_ROUND_NUMBER}"
echo "    LDB_REDUNDANT_DGROUPS: ${LDB_REDUNDANT_DGROUPS}"
echo "    LDB_DGROUP_BACK_PROPAGATION: ${LDB_DGROUP_BACK_PROPAGATION}"
echo "    OVERLAY: ${OVERLAY}"
echo "    SIMULATION: ${SIMULATION}"
echo "    NODE_NUMBER: ${NODE_NUMBER}"
echo "    NODE_EVENT_NUMBER: ${NODE_EVENT_NUMBER}"
echo "    PARTITION_NUMBER: ${PARTITION_NUMBER}"

# ENV SETUP:
# Kubernetes server and auth token
CONTEXT=$(kubectl config view |
          grep current |
          awk '{print $2}')
APISERVER=$(kubectl config view |
            grep "${CONTEXT}" -b1 |
            grep "server:" |
            grep -Eo "https://[0-9\.:]+")
TOKEN=$(kubectl describe secret |
        grep "token:" |
        awk '{print $2}')

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
LSIM_NAME=lsim-${TIMESTAMP}

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
        - name: BRANCH
          value: "${BRANCH}"
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
        - name: LDB_DRIVEN_MODE
          value: "${LDB_DRIVEN_MODE}"
        - name: LDB_STATE_SYNC_INTERVAL
          value: "${LDB_STATE_SYNC_INTERVAL}"
        - name: LDB_EVICTION_ROUND_NUMBER
          value: "${LDB_EVICTION_ROUND_NUMBER}"
        - name: LDB_REDUNDANT_DGROUPS
          value: "${LDB_REDUNDANT_DGROUPS}"
        - name: LDB_DGROUP_BACK_PROPAGATION
          value: "${LDB_DGROUP_BACK_PROPAGATION}"
        - name: OVERLAY
          value: "${OVERLAY}"
        - name: SIMULATION
          value: "${SIMULATION}"
        - name: NODE_NUMBER
          value: "${NODE_NUMBER}"
        - name: NODE_EVENT_NUMBER
          value: "${NODE_EVENT_NUMBER}"
        - name: PARTITION_NUMBER
          value: "${PARTITION_NUMBER}"
        - name: RSG
          value: "true"
---
apiVersion: extensions/v1beta1
kind: Deployment
metadata:
  name: "${LSIM_NAME}"
spec:
  replicas: ${NODE_NUMBER}
  template:
    metadata:
      labels:
        timestamp: "${TIMESTAMP}"
        tag: lsim
    spec:
      containers:
      - name: "${LSIM_NAME}"
        image: "${IMAGE}"
        imagePullPolicy: "${PULL_IMAGE}"
        securityContext:
          privileged: true
        env:
        - name: BRANCH
          value: "${BRANCH}"
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
        - name: LDB_DRIVEN_MODE
          value: "${LDB_DRIVEN_MODE}"
        - name: LDB_STATE_SYNC_INTERVAL
          value: "${LDB_STATE_SYNC_INTERVAL}"
        - name: LDB_EVICTION_ROUND_NUMBER
          value: "${LDB_EVICTION_ROUND_NUMBER}"
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
        - name: NODE_NUMBER
          value: "${NODE_NUMBER}"
        - name: NODE_EVENT_NUMBER
          value: "${NODE_EVENT_NUMBER}"
        - name: PARTITION_NUMBER
          value: "${PARTITION_NUMBER}"
        - name: KEEP_ALIVE
          value: "${KEEP_ALIVE}"
        - name: RSG
          value: "false"
EOF

kubectl create -f "${FILE}"

# wait time is number of events (each event is 1 second) plus 30 seconds
WAIT_TIME=$((NODE_EVENT_NUMBER + 60))

echo "[$(date +%T)] Waiting ${WAIT_TIME} second(s) before next deploy."
sleep ${WAIT_TIME}
