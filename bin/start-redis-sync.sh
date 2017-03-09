#!/usr/bin/env bash

POD_NAME=$(kubectl describe pod redis |
           grep -E "^Name:" |
           awk '{print $2}')

PORT=6379
DIR=$(dirname $0)
LOG_DIR=${DIR}/../priv/evaluation/logs

kubectl port-forward ${POD_NAME} ${PORT}:${PORT} & TUNNEL_PID=$!

echo "Port forwarding starting..."
sleep 3

LOG_DIR=$LOG_DIR ./$DIR/redis-sync.erl

echo "All files downloaded!"

kill ${TUNNEL_PID}

