#!/bin/bash

target0=${1:-http://localhost:9100/14333}
target1=${1:-http://localhost:9101/14333}
target2=${1:-http://localhost:9102/14333}

echo "*** Send requests to the cluster (3 nodes)"

for i in $(
  seq 5
); do
  echo ""
  echo "3 parallel requests to the server 0 with the same MSISDN"
  curl --parallel $target0 $target0 $target0
  echo ""
  echo "Parallel requests to the servers with the same MSISDN"
  curl --parallel $target0 $target1 $target2
  echo ""
  echo "Parallel requests to the servers with different MSISDN"
  curl --parallel $target0$i $target1$i $target2$i
done
