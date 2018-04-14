#!/bin/bash

END="500"

for i in $(seq 1 $END); do
  HITRATE=$(./paging-policy.py -f mem.txt -p FIFO -C $i -c -N | grep FINALSTATS | awk '{print $7}')
  echo "FIFO,$i,$HITRATE"
done

END="500"

for i in $(seq 1 $END); do
  HITRATE=$(./paging-policy.py -f mem.txt -p LRU -C $i -c -N | grep FINALSTATS | awk '{print $7}')
  echo "LRU,$i,$HITRATE"
done

END="500"

for i in $(seq 1 $END); do
  HITRATE=$(./paging-policy.py -f mem.txt -p MRU -C $i -c -N | grep FINALSTATS | awk '{print $7}')
  echo "MRU,$i,$HITRATE"
done

END="500"

for i in $(seq 1 $END); do
  HITRATE=$(./paging-policy.py -f mem.txt -p OPT -C $i -c -N | grep FINALSTATS | awk '{print $7}')
  echo "OPT,$i,$HITRATE"
done
