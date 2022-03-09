#!/bin/bash

trap 'echo "ERROR at line ${LINENO} (code: $?)" >&2' ERR
trap 'echo "Interrupted" >&2 ; exit 1' INT

set -o errexit
set -o nounset

# Ensure all alr runs are non-interactive
alias alr="alr -n"

# Detect changes
CHANGES=$(git diff --name-only HEAD~1)

# Bulk changes for the record
echo Changed files: $CHANGES

# Disable assistant. This is necessary despite the setup-alire action doing it
# too, because we sometimes run inside a Docker with fresh configuration
alr toolchain --disable-assistant

# Configure index
alr index --del local >/dev/null || true # Simplifies local testing
alr index --name local --add ./index

# Remove community index in case it has been added before
alr index --del community >/dev/null || true

function diff_one() {
    local file="$1"
    local folder=$(dirname $file)
    crate=$(basename $file .toml | cut -f1 -d-)
    version=$(basename $file .toml | cut -f2- -d-)
    milestone="$crate=$version"

    echo " "
    echo "------8<------"

    if echo $milestone | grep -q external; then
        echo DIFFING external: $milestone
        git diff HEAD~1 $file
    else
        echo DIFFING release: $milestone

        # Locate the immediately precedent release

        # For a first release, there's nothing to compare
        if [[ $(ls $folder | grep -vq external) -eq 1 ]]; then
            echo NOTHING to diff against, first crate release
            return 0
        fi

        # Othewise, get from alr what's the immediately preceding version
        local prev_milestone=$(alr show "$crate<$version" | head -1 | cut -f1 -d:)
        echo DIFFING milestones $prev_milestone '-->' $milestone

        # Convert into filename
        local prev_file=$folder/${prev_milestone//=/-}.toml

        git diff --no-index --minimal -U0 \
            --line-prefix "--| " \
            --ignore-all-space --ignore-blank-lines --ignore-cr-at-eol \
            $prev_file $file
    fi

    return 0
}

for file in $CHANGES; do
    diff_one "$file"
done
