#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")/../../gitlab-pages/website"
 
CURRENT_VERSION=$1

# retrieve version to delete from versions.json (last of the array)
VERSION_TO_DELETE=`jq '.[-1]' versions.json | tr -d '"'`

# Archive version
npm ci docusaurus
npm run docusaurus docs:version $CURRENT_VERSION


############################################
# REMOVE OLDEST VERSION
# We want to keep only 3 versions
############################################

# MAVRYK: with the frozen 1.6.0 snapshot retired, versions.json is [] so `jq '.[-1]'` above yields "null"
# and there is no oldest snapshot to prune yet. Guard the prune so it only runs once a real oldest version
# exists (avoids a stray `rm` error and an accidental version-null path). This restores the original
# "keep only 3 versions" behaviour automatically once 4+ versions accumulate again after future releases.
if [ -n "$VERSION_TO_DELETE" ] && [ "$VERSION_TO_DELETE" != "null" ]; then
  # versioned_docs/version-$VERSION_TO_DELETE
  rm -rf "versioned_docs/version-$VERSION_TO_DELETE"
  # versioned_sidebars/version-$VERSION_TO_DELETE-sidebars.json
  rm -f "versioned_sidebars/version-$VERSION_TO_DELETE-sidebars.json"
  # versions.json (delete last entry which represent the oldest version)
  jq "del(.[3])"  versions.json > versions.tmp && mv versions.tmp versions.json
else
  echo "MAVRYK: no oldest version to prune (versions.json empty/null); skipping snapshot removal."
fi
