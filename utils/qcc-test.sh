#!/usr/bin/env bash

# set -e

zig build-exe -freference-trace qcc.zig
mv qcc /tmp/

total=0
pass=0
for qcfiles in $(ls -Sr ../tests/quakec/*.qc ../tests/v101qc/*.qc)
do
  if [[ "${qcfiles}" = "../tests/quakec/enum.qc" ]];
  then
    continue
  fi
  echo -n "${qcfiles}... "
  output_filename=/tmp/$(basename ${qcfiles}).output
  /tmp/qcc ${qcfiles} 2>&1 > ${output_filename}
  res=$?
  if [ $res -eq 0 ]
  then
    echo "OK"
    pass=$((pass+1))
  else
    echo "KO"
    cat ${output_filename}
  fi
  total=$((total+1))
done
echo ${pass}/${total} tests
