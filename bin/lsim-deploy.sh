#!/usr/bin/env bash

ENV_VARS=(
  BRANCH
  NODE_NUMBER
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
SERVERAPI=$(kubectl config view |
            grep "server:" |
            grep -Eo "https://[0-9\.:]+")
TOKEN=$(kubectl describe secret |
        grep "token:" |
        sed -E 's/token:\s+//')

# Evaluation timestamp: unix timestamp + nanoseconds
TIMESTAMP=$(date +%s)$(date +%N)

# Port
PEER_PORT=6866

# DEPLOYMENT:
# Deployment name
NAME=lsim-${TIMESTAMP}

# Docker image
IMAGE=vitorenesduarte/lsim

# YAML file
FILE=/tmp/$NAME.yaml

cat <<EOF > $FILE
apiVersion: extensions/v1beta1
kind: Deployment
metadata:
  name: "${NAME}"
spec:
  replicas: ${NODE_NUMBER}
  template:
    metadata:
      labels:
        timestamp: "${TIMESTAMP}"
    spec:
      containers:
      - name: "${NAME}"
        image: "${IMAGE}"
        imagePullPolicy: IfNotPresent
        env:
        - name: BRANCH
          value: "${BRANCH}"
        - name: IP
          valueFrom:
            fieldRef:
              fieldPath: status.podIP 
        - name: PEER_PORT
          value: "${PEER_PORT}"
        - name: SERVERAPI
          value: "${SERVERAPI}"
        - name: TOKEN
          value: "${TOKEN}"
        - name: TIMESTAMP
          value: "${TIMESTAMP}"
EOF

echo "Creating deployment."
kubectl create -f $FILE
