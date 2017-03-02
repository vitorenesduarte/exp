#!/usr/bin/env bash

ENV_VARS=(
  BRANCH
  LDB_MODE
  LDB_JOIN_DECOMPOSITIONS
  OVERLAY
  SIMULATION
  NODE_NUMBER
  NODE_EVENT_NUMBER
)

for ENV_VAR in "${ENV_VARS[@]}"
do
  if [ -z "${!ENV_VAR}" ]; then
    echo ">>> ${ENV_VAR} is not configured; please export it."
    exit 1
  fi
done

# ENV SETUP:
# Kubernetes server and auth token
APISERVER=$(kubectl config view |
            grep "server:" |
            grep -Eo "https://[0-9\.:]+")
TOKEN=$(kubectl describe secret |
        grep "token:" |
        sed -E 's/token:\s+//')

ORCHESTRATION=kubernetes

# Evaluation timestamp: unix timestamp + nanoseconds
TIMESTAMP=$(date +%s)$(date +%N)

# Port
PEER_PORT=6866

# DEPLOYMENT:
# Deployment names
LSIM_NAME=lsim-${TIMESTAMP}
RSG_NAME=rsg-${TIMESTAMP}

# Docker image
IMAGE=vitorenesduarte/lsim

# YAML file
FILE=/tmp/$NAME.yaml

cat <<EOF > $FILE
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
        imagePullPolicy: IfNotPresent
        env:
        - name: BRANCH
          value: "${BRANCH}"
        - name: ORCHESTRATION
          value: "${ORCHESTRATION}"
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
        - name: LDB_JOIN_DECOMPOSITIONS
          value: "${LDB_JOIN_DECOMPOSITIONS}"
        - name: OVERLAY
          value: "${OVERLAY}"
        - name: SIMULATION
          value: "${SIMULATION}"
        - name: NODE_NUMBER
          value: "${NODE_NUMBER}"
        - name: NODE_EVENT_NUMBER
          value: "${NODE_EVENT_NUMBER}"
        - name: RSG
          value: "false"
---
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
        imagePullPolicy: IfNotPresent
        env:
        - name: BRANCH
          value: "${BRANCH}"
        - name: ORCHESTRATION
          value: "${ORCHESTRATION}"
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
        - name: NODE_NUMBER
          value: "${NODE_NUMBER}"
        - name: RSG
          value: "true"
EOF

echo "Creating deployment."
kubectl create -f $FILE
