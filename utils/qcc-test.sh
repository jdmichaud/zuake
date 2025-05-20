#!/usr/bin/env bash

set -e

zig build-exe -freference-trace qcc.zig
mv qcc /tmp/

for qcfiles in $(ls -Sr ../tests/quakec/*.qc)
do
  echo -n "..${qcfiles}... "
  output_filename=/tmp/$(basename ${qcfiles}).output
  /tmp/qcc ${qcfiles} 2>&1 > ${output_filename}
  res=$?
  if [ $res -eq 0 ]
  then
    echo "OK"
  else
    echo "KO"
    cat ${output_filename}
  fi
done