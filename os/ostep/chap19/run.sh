#!/bin/bash

for attempt in $(seq 15); do
  pages=$(echo "2^$attempt" | bc)
  ./measure "$pages" 100
done
