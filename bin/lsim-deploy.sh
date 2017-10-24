#!/usr/bin/env bash

ENV_VARS=(
  BRANCH
  IMAGE
  PULL_IMAGE
  SIMULATION
  NODE_NUMBER
  NODE_EVENT_NUMBER
  ELEMENT_NODE_RATIO
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
echo "    SIMULATION: ${SIMULATION}"
echo "    NODE_NUMBER: ${NODE_NUMBER}"
echo "    NODE_EVENT_NUMBER: ${NODE_EVENT_NUMBER}"
echo "    ELEMENT_NODE_RATIO: ${ELEMENT_NODE_RATIO}"
echo "    PARTITION_NUMBER: ${PARTITION_NUMBER}"

# ENV SETUP:
# Kubernetes server and auth token
CONTEXT=$(kubectl config view |
          grep current |
          awk '{print $2}')
APISERVER=$(kubectl config view |
            grep -Eb1 "${CONTEXT}$" |
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
        - name: SIMULATION
          value: "${SIMULATION}"
        - name: NODE_NUMBER
          value: "${NODE_NUMBER}"
        - name: NODE_EVENT_NUMBER
          value: "${NODE_EVENT_NUMBER}"
        - name: ELEMENT_NODE_RATIO
          value: "${ELEMENT_NODE_RATIO}"
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
# Enabling Unsafe Sysctls:
# - Docs: https://kubernetes.io/docs/concepts/cluster-administration/sysctl-cluster/#safe-vs-unsafe-sysctls
# - PR:   https://github.com/kubernetes/kubernetes/pull/26057/files?short_path=8dc23ab#diff-8dc23ab258695ee42154d4d1238c36ef
# - Help: http://www.ehowstuff.com/configure-linux-tcp-keepalive-setting/
#      annotations:
#        security.alpha.kubernetes.io/unsafe-sysctls: net.ipv4.tcp_keepalive_time=10,net.ipv4.tcp_keepalive_intvl=5,net.ipv4.tcp_keepalive_probes=1
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
        # resources:
        #   requests:
        #     memory: "64Mi"
        #     cpu: "250m"
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
        - name: SIMULATION
          value: "${SIMULATION}"
        - name: NODE_NUMBER
          value: "${NODE_NUMBER}"
        - name: NODE_EVENT_NUMBER
          value: "${NODE_EVENT_NUMBER}"
        - name: ELEMENT_NODE_RATIO
          value: "${ELEMENT_NODE_RATIO}"
        - name: PARTITION_NUMBER
          value: "${PARTITION_NUMBER}"
        - name: KEEP_ALIVE
          value: "${KEEP_ALIVE}"
        - name: RSG
          value: "false"
EOF

kubectl create -f "${FILE}"

# wait time is number of events (each event is 1 second) plus 60 seconds
WAIT_TIME=$((NODE_EVENT_NUMBER + 60))

echo "[$(date +%T)] Waiting ${WAIT_TIME} second(s) before next deploy."
sleep ${WAIT_TIME}
