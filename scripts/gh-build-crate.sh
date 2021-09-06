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

# Disable assistant. This is necessary despite the setup-alire action doing it
# too, because we sometimes run inside a Docker with fresh configuration
alr toolchain --disable-assistant

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
   is_system=false

   crate=$(basename $file .toml | cut -f1 -d-)
   echo Testing crate: $crate

   # Show info for the record
   echo PLATFORM-INDEPENDENT CRATE INFO $crate
   alr show $crate
   alr show --external $crate
   alr show --external-detect $crate

   echo PLATFORM-DEPENDENT CRATE INFO $crate
   alr show --system $crate
   alr show --external --system $crate
   alr show --external-detect --system $crate
   crateinfo=$(alr show --external-detect --system $crate)

   echo CRATE DEPENDENCIES $crate
   alr show --solve --detail --external-detect $crate
   solution=$(alr show --solve --detail --external-detect $crate)

   # Skip on explicit unavailability
   if alr show --system $crate | grep -q 'Available when: False'; then
      echo SKIPPING crate build: $crate UNAVAILABLE on system
      continue
   fi

   # In unsupported platforms, externals are properly reported as missing. We
   # can skip testing of such a crate since it will likely fail.
   if grep -q 'Dependencies (external):' <<< $solution ; then
      echo SKIPPING build for crate $crate with MISSING external dependencies
      continue
   fi

   # Update system repositories whenever a detected system package is involved, 
   # either as dependency or as the crate being tested.
   if grep -iq 'origin: system' <<< $solution; then
      echo UPDATING system repositories...
      type apt-get 2>/dev/null && apt-get update || true
      type pacman  2>/dev/null && pacman -Syy    || true
   else
      echo No need to update system repositories
   fi

   # Detect whether the crate is binary to skip build
   is_binary=false
   if grep -iq 'binary archive' <<< $crateinfo; then
      echo Crate is BINARY
      is_binary=true
   fi

   # Alternatives for when the crate itself comes from an external. Only system
   # externals should be tested.
   if grep -q 'Origin: external path' <<< $crateinfo ; then
      echo SKIPPING detected external crate $crate
      continue
   elif grep -q 'Origin: system package' <<< $crateinfo ; then
      echo INSTALLING detected system crate $crate
      is_system=true
   elif grep -q 'Not found:' <<< $crateinfo && \
        grep -q 'There are external definitions' <<< $crateinfo
   then
      echo SKIPPING undetected external crate $crate
      continue
   fi

   # Detect missing dependencies for clearer error
   if grep -q 'Dependencies cannot be met' <<< $solution ; then
      echo FAIL: crate $crate dependencies cannot be met
      exit 1
   fi
   
   # Actual checks
   echo DEPLOYING CRATE $crate
   if $is_binary; then 
      echo SKIPPING BUILD for BINARY crate, FETCHING only
      build_flag=""
   else
      build_flag="--build"
   fi

   alr get -d $build_flag -n $crate

   if $is_system; then 
      echo DETECTING INSTALLED PACKAGE via crate $crate
      alr show -d --external-detect $crate
   elif $is_binary; then
      echo FETCHED BINARY crate OK
   else
      echo LISTING EXECUTABLES of crate $crate
      cd ${crate}_*
      alr run -d --list
      cd ..
   fi

   echo CRATE $crate TEST ENDED SUCCESSFULLY
done
