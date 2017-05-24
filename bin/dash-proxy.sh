#!/usr/bin/env bash

UNAME=$(uname)

POD_NAME=$(kubectl get pods |
           grep lsim-dash |
           grep Running |
           awk '{print $1}')

POD_PORT=3000
PORT=$RANDOM

if [[ "$UNAME" == 'Darwin' ]]; then
	open -a 'Google Chrome' http://localhost:${PORT}
else
	google-chrome http://localhost:${PORT}
fi

kubectl port-forward "${POD_NAME}" ${PORT}:${POD_PORT}