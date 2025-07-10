#!/bin/bash

# Creates output directories for the compiler project.  Because the CS network
# is an NFS and has subtle sync issues, we'll put the compiler's output in a
# shared memory filesystem if we can.
for dir in output logs _build; do
    if [ ! -d "$dir" ]; then
        if [ -d /dev/shm ]; then
            name="/dev/shm/compiler-$(id -u -n)-$dir-$(basename "$(pwd)")"
            rm -rf "$name"
            mkdir -p -m '0700' "$name"
            if [ ! -e "$dir" ]; then
              ln -s "$name" "$dir"
            fi
        else
            mkdir "$dir"
        fi
    fi
done
