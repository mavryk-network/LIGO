#!/usr/bin/env nix-shell
#!nix-shell -p mustache-go jq  -i bash

# shellcheck shell=bash

cd "$(dirname "${BASH_SOURCE[0]}"/..)"

CHANGELOG_ENTRY_FOLDER=${1:-"changelog"}

./scripts/changelog-json.sh $CHANGELOG_ENTRY_FOLDER > changelog.json

jq '
  .changelog[].changes |=
  with_entries (
    if .key | in({"breaking":1,"added":1,"fixed":1,"changed":1,"deprecated":1,"removed":1,"performance":1,"internal":1,"other":1,"null":1})
    then
      .value[].description |=
      if . != null
      then
        gsub("\n";"\n  ")
      else
        .
      end
    else
      .
    end
  )' \
< changelog.json \
> changelog_indented.json

mustache ./changelog_indented.json ./scripts/changelog.md.mustache > changelog.md
mustache ./changelog.json ./scripts/changelog.txt.mustache > changelog.txt

jq '.changelog[0]' changelog.json > release-notes.json

mustache ./release-notes.json ./scripts/release-notes.md.mustache > release-notes.md
mustache ./release-notes.json ./scripts/release-notes.txt.mustache > release-notes.txt

