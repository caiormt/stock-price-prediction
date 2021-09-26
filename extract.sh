#!/bin/sh

echo "Starting..."

rm prediction/resources/COTAHIST_BOVA11.txt

for i in {10..20}; do
  echo "Extracting 20$i ..."
  zcat "prediction/resources/COTAHIST_A20$i.ZIP" | grep "BOVA11 " >>prediction/resources/COTAHIST_BOVA11.txt
done

echo "Done!"
