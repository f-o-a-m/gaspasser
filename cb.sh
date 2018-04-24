#!/bin/sh
docker kill watchy-cb > /dev/null
docker rm watchy-cb > /dev/null 
echo restarting cliquebait: 
docker run --name watchy-cb -d --rm -p 8545:8545 -v `pwd`/cliquebait-generated.json:/cliquebait/cliquebait.json -e ACCOUNTS_TO_CREATE=5 foamspace/cliquebait:latest
echo
