# How to build:
#
# docker build ../../ -f Dockerfile --build-arg EXAMPLES_DIR_SRC=src/test/examples
FROM node:16-buster-slim as stage1

ARG EXAMPLES_DIR_SRC
ARG EXAMPLES_DIR_DEST=packages/client/examples
ARG GITLAB_TOKEN="AKeHhbr-3_2p-mkaah3C"

RUN apt-get update && apt-get -y install perl6 libev-dev perl pkg-config libgmp-dev libhidapi-dev m4 libcap-dev bubblewrap rsync jq curl python3 build-essential

# Download the past 5 releases
FROM stage1 as stage2
WORKDIR /app
COPY tools/webide/bin/fetch_ligo_releases.sh fetch_ligo_releases.sh
ENV GITLAB_TOKEN=$GITLAB_TOKEN
ENV WORKING_DIR="/app"
RUN /app/fetch_ligo_releases.sh
RUN rm /app/fetch_ligo_releases.sh

# Copy source
FROM stage2 as stage3
COPY tools/webide/package.json package.json
COPY tools/webide/yarn.lock yarn.lock
COPY tools/webide/tsconfig.json tsconfig.json
COPY tools/webide/packages/client packages/client
COPY tools/webide/packages/server packages/server
COPY $EXAMPLES_DIR_SRC/ packages/client/examples

# Build
FROM stage3 as stage4
RUN yarn
FROM stage4 as stage5
ENV EXAMPLES_DIR="/app/packages/client/examples"
RUN yarn workspace client run build
FROM stage5 as stage6
RUN yarn workspace server run build

# Copy build to app
FROM stage6 as stage7
COPY --from=stage5 /app/packages/client/build /app/client/build
COPY --from=stage5 /app/packages/server/dist/src /app/server/dist

# Run the backend server
EXPOSE 8080
ENV STATIC_ASSETS /app/client
ENV LIGO_CMD /bin/ligo
ENTRYPOINT [ "node", "/app/server/dist/index.js" ]
