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

# Show alr metadata
alr version

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

   echo CRATE DEPENDENCIES
   solution=$(alr show --solve $crate)
   echo $solution

   # Skip on explicit unavailability
   if $(alr show --system | grep -q 'Available when: False'); then
      echo SKIPPING crate build: UNAVAILABLE on system
      continue
   fi

   # In unsupported platforms, externals are properly reported as missing. We
   # can skip testing of such a crate since it will likely fail.
   if $(echo $solution | grep -q 'Dependencies (external):'); then
      echo SKIPPING build for crate with MISSING external dependencies
      continue
   fi

   # TODO: Ideally we should do this only when we have a system crate in the
   # mix. There's no simple way to know this at present though.
   echo Updating system repositories...
   type apt-get 2>/dev/null && apt-get update || true
   type pacman  2>/dev/null && pacman -Syy    || true

   # Detect missing dependencies for clearer error
   if $(echo $solution | grep -q 'Dependencies cannot be met'); then
      echo FAIL: crate dependencies cannot be met
      exit 1
   fi
   
   # Actual checks
   echo BUILDING CRATE
   alr get --build -n $crate

   echo LISTING EXECUTABLES
   cd ${crate}_*
   alr run --list
   cd ..

   echo CRATE BUILD ENDED SUCCESSFULLY
done
