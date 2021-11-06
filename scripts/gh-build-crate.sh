#!/bin/bash

trap 'echo "ERROR at line ${LINENO} (code: $?)" >&2' ERR
trap 'echo "Interrupted" >&2 ; exit 1' INT

set -o errexit
set -o nounset

function check_dependencies_exist() {
   for dep in $(alr with | tail +1); do
      if [[ $dep =~ ([a-z0-9_]+)[^a-z0-9_] ]]; then # this should always match the crate name
         local crate=${BASH_REMATCH[1]}
         
         if alr list | cut -f1 -d' ' | grep -qw $crate; then
            return 0 # found
         else
            echo "Direct dependency is unknown: $dep (crate name: $crate)"
            return 1
         fi
      else
         echo Could not identify dependency crate name in: $dep
         return 1
      fi
   done

   return 0
}

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

# Remove community index in case it has been added before
alr index --del community || true

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

   if ! [ -f ./"$file" ]; then
      echo Skipping deleted file: $file
      continue
   fi

   # Checks passed, this is a crate we must test
   is_system=false

   crate=$(basename $file .toml | cut -f1 -d-)
   version=$(basename $file .toml | cut -f2- -d-)
   version_noextras=$(echo $version | cut -f1 -d- | cut -f1 -d+)
   milestone="$crate=$version"
   echo Testing crate: $milestone
   # Remember that version can be "external", in which case we do not know the
   # actual version, and indeed the test will only work if the external is the
   # newest version. This probably merits a way of being tested properly, but
   # that will require changes in alr.

   if [[ $version = external ]]; then
      echo Downgrading milestone to plain crate name
      milestone=$crate
   fi

   # Show info for the record
   echo PLATFORM-INDEPENDENT CRATE INFO $milestone
   alr show $milestone
   alr show --external $milestone
   alr show --external-detect $milestone

   echo PLATFORM-DEPENDENT CRATE INFO $milestone
   alr show --system $milestone
   alr show --external --system $milestone
   alr show --external-detect --system $milestone
   crateinfo=$(alr show --external-detect --system $milestone)

   echo CRATE DEPENDENCIES $milestone
   alr show --solve --detail --external-detect $milestone
   solution=$(alr show --solve --detail --external-detect $milestone)

   # Check that no direct dependency is unknown
   check_dependencies_exist()

   # Skip on explicit unavailability
   if alr show --system $milestone | grep -q 'Available when: False'; then
      echo SKIPPING crate build: $milestone UNAVAILABLE on system
      continue
   fi

   # In unsupported platforms, externals are properly reported as missing. We
   # can skip testing of such a crate since it will likely fail.
   if grep -q 'Dependencies (external):' <<< $solution ; then
      echo SKIPPING build for crate $milestone with MISSING external dependencies
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
      echo SKIPPING detected external crate $milestone
      continue
   elif grep -q 'Origin: system package' <<< $crateinfo ; then
      echo INSTALLING detected system crate $milestone
      is_system=true
   elif grep -q 'Not found:' <<< $crateinfo && \
        grep -q 'There are external definitions' <<< $crateinfo
   then
      echo SKIPPING undetected external crate $crate
      continue
   fi

   # Detect missing dependencies for clearer error
   if grep -q 'Dependencies cannot be met' <<< $solution ; then
      echo FAIL: crate $milestone dependencies cannot be met
      exit 1
   fi

   # Actual checks
   echo DEPLOYING CRATE $milestone
   if $is_binary; then
      echo SKIPPING BUILD for BINARY crate, FETCHING only
   elif $is_system; then
      echo SKIPPING BUILD for SYSTEM crate, FETCHING only
   fi

   alr -d -n get $milestone

   if $is_system; then
      echo DETECTING INSTALLED PACKAGE via crate $milestone
      alr -d show --external-detect $milestone
   elif $is_binary; then
      echo FETCHED BINARY crate OK
   else
      echo FETCHED SOURCE crate OK
      cd ${crate}_${version_noextras}_*
      echo BUILD ENVIRONMENT
      alr printenv
      echo BUILDING CRATE
      alr -d -n build 
      echo LISTING EXECUTABLES of crate $milestone
      alr -d run --list
      cd ..
   fi

   echo CRATE $milestone TEST ENDED SUCCESSFULLY
done
