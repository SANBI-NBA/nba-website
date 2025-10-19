#!/bin/bash

# Go through each subdirectory in the current folder
for dir in */; do
  if [ -d "$dir/.git" ]; then
    echo "Updating repository: $dir"
    cd "$dir" || continue

    # Fetch and update main branch
    git checkout main && git pull origin main

    cd ..
    echo "--------------------------------------"
  else
    echo "Skipping $dir — not a Git repository."
  fi
done