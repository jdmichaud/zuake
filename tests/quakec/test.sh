#!/usr/bin/env bash

# Compile the examples (need gmqcc in the path):
# ls *.qc | xargs -n1 -i bash -c 'gmqcc -std=gmqcc -O0 "$1" -o "${1%.qc}.dat"' - '{}'

if [ $# -ne 1 ]
then
  echo "error: expecting the path to qvm binary"
  exit 1
fi
qvm=$1

executed=0
failed=0

tmpfile1=$(mktemp)
tmpfile2=$(mktemp)
tmpdiff=$(mktemp)

for testfile in $(ls *.test)
do
  fail=0
  executed=$((executed+1))
  cmd=$(grep cmd $testfile | awk -F':' '{ print $2 }')
  binary=$(grep file $testfile | awk -F':' '{ print $2 }')
  echo -n $binary
  exitcode=$(grep exit $testfile | awk -F':' '{ print $2 }')
  $qvm $cmd $binary > $tmpfile1 2>&1
  tail +5 $testfile > $tmpfile2
  observed=$?
  if [ $observed -ne $exitcode ]
  then
    fail=1
    echo -n " incorrect exit code"
  fi

  diff $tmpfile1 $tmpfile2 > ${tmpdiff}
  if [ $? -ne 0 ]
  then
    fail=1
    echo -n " incorrect output"
  fi

  if [ $fail -ne 0 ]
  then
    failed=$((failed+1))
    echo ""
    cat ${tmpdiff}
  else
    echo " OK"
  fi
done

echo "$executed executed $failed failed"

if [ $failed -gt 0 ]
then
  exit 1
fi
