BRANCH=master \
LDB_MODE=state_based \
LDB_JOIN_DECOMPOSITIONS=false \
OVERLAY=ring \
SIMULATION=gset \
NODE_NUMBER=3 \
NODE_EVENT_NUMBER=50 ./bin/lsim-deploy.sh

BRANCH=master \
LDB_MODE=delta_based \
LDB_JOIN_DECOMPOSITIONS=false \
OVERLAY=ring \
SIMULATION=gset \
NODE_NUMBER=3 \
NODE_EVENT_NUMBER=50 ./bin/lsim-deploy.sh

BRANCH=master \
LDB_MODE=delta_based \
LDB_JOIN_DECOMPOSITIONS=true \
OVERLAY=ring \
SIMULATION=gset \
NODE_NUMBER=3 \
NODE_EVENT_NUMBER=50 ./bin/lsim-deploy.sh

