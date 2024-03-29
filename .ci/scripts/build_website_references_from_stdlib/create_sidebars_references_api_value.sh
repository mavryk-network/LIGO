#!/usr/bin/env bash
cd "$(dirname "${BASH_SOURCE[0]}")"

prefix=$1
docusaurus_prefix=reference/
sidebar_path=$2

get_id_of_file() {
  echo $(head -n 2 $1 | grep '^id:' | cut -d ':' -f2 | xargs)
}

build_files_items() {
  local files_items=""
  for file in "$@"; do
    id=$(get_id_of_file $file)
    files_items="$files_items \"reference/$id\",\\n"
  done
  echo "$files_items"
}

build_folders_items() {
  local folders_items=""

  for folder in "${folders[@]}"; do
    folder_basename=$(basename $folder)
    folders_items="$folders_items${initial_folder_template//<<label>>/\"$folder_basename\"}"
    ## Fill items section
    local items=""
    local files=($(find $folder -mindepth 1 -maxdepth 1 -type f))

    file_items=$(build_files_items "${files[@]}")
    # Remove the last ,\n
    file_items=${file_items:0:-3}

    items="$items$file_items"

    local folders=($(find $folder -mindepth 1 -maxdepth 1 -type d))
    if [[ ${#folders[@]} -ne 0 ]]; then
      items="$items,\\n $(build_folders_items "${folders[@]}")"
    fi
    folders_items="${folders_items//<<items>>/$items},\\n"
  done
  #replace pattern },] by }]
  folders_items=${folders_items:0:-3}
  echo "$folders_items"
}

folders=($(find $prefix -mindepth 1 -maxdepth 1 -type d))
files=($(find $prefix -mindepth 1 -maxdepth 1 -type f))
initial_folder_template=$(cat docusaurus_sidebar_category_template)
#this variable will be the first layer of the builded sidebar item
first_layer="[\\n"
##### Build the list from filename #####
first_layer="$first_layer $(build_files_items "${files[@]}")"

##### Build the list from folder #####
first_layer="$first_layer $(build_folders_items "${folders[@]}")"

##### Format the closing json object #####
# Add the ] to close the json array
first_layer="\\n$first_layer\\n]"

# Create js script which export the new json value of API tree
echo -e "/** Generated by script */\nconst api_value = $first_layer\n\nmodule.exports = api_value;" > api_value.js

node update_sidebars.js $sidebar_path
