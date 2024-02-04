#!/usr/bin/env bash

shopt -s extglob
rm -- !(*.fsproj|*.fs|*.fsx|*.sh|.gitignore|README.md)
