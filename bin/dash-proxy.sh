#!/usr/bin/env bash

POD_NAME=$(kubectl get pods |
           grep lsim-dash |
           awk '{print $1}')

PORT=3000
kubectl port-forward "${POD_NAME}" ${PORT}:${PORT}

