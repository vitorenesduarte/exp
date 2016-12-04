#!/bin/bash

DCOS=$(dcos config show core.dcos_url)
TOKEN=$(dcos config show core.dcos_acs_token)

ENV_VARS=(
  LDB_MODE
  LDB_JOIN_DECOMPOSITIONS
  LDB_EXTENDED_LOGGING
  BRANCH
  OVERLAY
  SIMULATION
  NODE_NUMBER
  EVALUATION_IDENTIFIER
  EVALUATION_TIMESTAMP
)

for ENV_VAR in "${ENV_VARS[@]}"
do
  if [ -z "${!ENV_VAR}" ]; then
    echo ">>> ${ENV_VAR} is not configured. Please export it."
    exit 1
  fi
done

echo ">>> Configuring lsims"
cd /tmp

MEMORY=512.0
CPU=0.6

cat <<EOF > lsim.json
{
  "acceptedResourceRoles": [
    "slave_public"
  ],
  "id": "lsims",
  "dependencies": [],
  "cpus": $CPU,
  "mem": $MEMORY,
  "instances": $NODE_NUMBER,
  "container": {
    "type": "DOCKER",
    "docker": {
      "image": "vitorenesduarte/lsim",
      "network": "HOST",
      "forcePullImage": true,
      "parameters" : [
        { "key": "oom-kill-disable", "value": "true" }
      ]
    }
  },
  "ports": [0],
  "env" : {
    "DCOS": "$DCOS",
    "TOKEN": "$TOKEN",
    "LDB_MODE": "$LDB_MODE",
    "LDB_JOIN_DECOMPOSITIONS": "$LDB_JOIN_DECOMPOSITIONS",
    "LDB_EXTENDED_LOGGING": "$LDB_EXTENDED_LOGGING",
    "BRANCH": "$BRANCH",
    "OVERLAY": "$DCOS_OVERLAY",
    "SIMULATION": "$SIMULATION",
    "NODE_NUMBER": "$NODE_NUMBER",
    "EVALUATION_IDENTIFIER": "$EVALUATION_IDENTIFIER",
    "EVALUATION_TIMESTAMP": "$EVALUATION_TIMESTAMP"
  },
  "healthChecks": []
}
EOF

echo ">>> Adding lsims to Marathon"
curl -s -H "Authorization: token=$TOKEN" -H 'Content-type: application/json' -X POST -d @lsims.json "$DCOS/service/marathon/v2/apps" > /dev/null
