#!/bin/bash

curl -o /tmp/releases.json "https://gitlab.com/api/v4/projects/12294987/releases/" -H 'Accept: application/json' -H "PRIVATE-TOKEN: ${GITLAB_TOKEN}"
cat /tmp/releases.json | jq -c '.[] | {version: .tag_name, downloadUrl: .assets.links[1].url}' > "${WORKING_DIR}/ligo-releases.json"
for release in `cat ${WORKING_DIR}/ligo-releases.json | head -5`; do
    LIGO_VERSION=`echo ${release} | jq -r .version`
    LIGO_URL=`echo ${release} | jq -r .downloadUrl`
    LIGO_CMD="/bin/ligo-${LIGO_VERSION}"
    echo "Downloading LIGO v${LIGO_VERSION}"
    curl --silent -o $LIGO_CMD $LIGO_URL
    chmod a+x $LIGO_CMD
done

LATEST=`cat ${WORKING_DIR}/ligo-releases.json | head -1`
LIGO_VERSION=`echo ${LATEST} | jq -r .version`
LIGO_CMD="/bin/ligo-${LIGO_VERSION}"
ln -sf LIGO_CMD /bin/ligo
