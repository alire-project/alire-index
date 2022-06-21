#!/bin/bash

trap 'echo "ERROR at line ${LINENO} (code: $?)" >&2' ERR
trap 'echo "Interrupted" >&2 ; exit 1' INT

set -o errexit
set -o nounset

# Ensure all alr runs are non-interactive and able to output unexpected errors
alias alr="alr -d -n"

# Disable check for ownership that sometimes confuses docker-run git
git config --global --add safe.directory '*'

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

# Show environment for the record
env

# Check index for obsolescent features
echo STRICT MODE index checks
alr index --check

# Check no warning during index loading.
# Such a warning would also happen during `alr printenv`, breaking it.
# TODO: remove after old license deprecation.
alr search --crates 2>&1 | grep "Warning:" && exit 1

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

   # Fail if there are pins in the manifest
   if grep -q 'Pins (direct)' <<< $crateinfo ; then
      echo "FAIL: release $milestone manifest contains pins"
      exit 1
   fi

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
      echo "UPDATING system repositories with sudo from user ${USERNAME:-unset} ($UID:-unset)..."
      type apt-get 2>/dev/null && sudo apt-get update || true
      type pacman  2>/dev/null && sudo pacman -Syy    || true
   else
      echo No need to update system repositories
   fi

   # Install an Alire-provided gprbuild whenever there is a non-external gnat in solution
   if grep -iq 'gnat_' <<< $solution && ! grep -iq 'gnat_external' <<< $solution; then
      gnat_dep=$(grep -E -o '^   gnat_[a-z0-9_]*=\S*' <<< $solution | tail -1 | xargs)
      gnat_dep=${gnat_dep:-gnat_native}
      echo "INSTALLING indexed gprbuild compatible with $gnat_dep"
      alr toolchain --select $gnat_dep gprbuild
      # -E for regex, -o for only the matched part, xargs to trim space
      # We must give both the gnat in the solution and gprbuild, so both are compatible
      # Even if we default to gnat_native, that would select the appropriate gprbuild
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

   alr -q get $milestone

   if $is_system; then
      echo DETECTING INSTALLED PACKAGE via crate $milestone
      alr show --external-detect $milestone
   elif $is_binary; then
      echo FETCHED BINARY crate OK
   else
      echo FETCHED SOURCE crate OK, deployed at $(alr get --dirname $milestone)

      # Enter the deployment dir
      cd $(alr get --dirname $milestone)

      echo BUILD ENVIRONMENT
      alr printenv

      echo BUILDING CRATE
      alr build --release
      # As normally dependencies/executables are built in release mode, we also
      # check any submissions in this mode. Should we go overboard and check the
      # three profile modes?

      echo LISTING EXECUTABLES of crate $milestone
      alr run --list

      cd ..
   fi

   echo CRATE $milestone TEST ENDED SUCCESSFULLY
done
