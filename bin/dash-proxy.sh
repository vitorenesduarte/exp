#!/usr/bin/env bash

POD_NAME=$(kubectl get pods |
           grep lsim-dash |
           grep Running |
           awk '{print $1}')

POD_PORT=3000
PORT=$RANDOM
open "http://localhost:${PORT}"
kubectl port-forward "${POD_NAME}" ${PORT}:${POD_PORT}

