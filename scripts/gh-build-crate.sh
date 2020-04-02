#!/bin/bash

trap 'echo "ERROR at line ${LINENO} (code: $?)" >&2' ERR
trap 'echo "Interrupted" >&2 ; exit 1' INT

set -o errexit
set -o nounset

# See whats happening
git log --graph --decorate --pretty=oneline --abbrev-commit --all | head -30

# Detect changes
CHANGES=$(git diff --name-only HEAD~1)

# Bulk changes for the record
echo Changed files: $CHANGES

# Import the out-of-docker built alr
export PATH+=:${PWD}/alire/bin

# Configure index
alr index --name local --add ./index

# Test crate
for file in $CHANGES; do

   if [[ $file == index.toml ]]; then 
      echo Skipping index metadata file: $file
      continue
   fi

   if [[ $file != *.toml ]]; then
      echo Skipping non-crate file: $file
      continue
   fi

   if ! [ -f ./$file ]; then
      echo Skipping deleted file: $file
      continue
   fi

   # Checks passed, this is a crate we must test

   crate=$(basename $file .toml)
   echo Testing crate: $crate

   # Show info for the record
   echo PLATFORM-INDEPENDENT CRATE INFO
   alr show $crate
   alr show --external $crate
   alr show --external-detect $crate

   echo PLATFORM-DEPENDENT CRATE INFO
   alr show --system $crate
   alr show --external --system $crate
   alr show --external-detect --system $crate

   if $(alr show $crate --system | grep -q 'Available when: False'); then
      echo Skipping crate build: UNAVAILABLE on system
      continue
   fi

   echo BUILDING CRATE
   alr get --build -n $crate

   echo LISTING EXECUTABLES
   cd ${crate}_*
   alr run --list
   cd ..

   echo CRATE BUILD ENDED SUCCESSFULLY
done
