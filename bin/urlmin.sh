#!/bin/bash
# A simple url shortening script using bit.ly - make sure to include your api
# keys bellow.
# Author: yamadapc <github.com/yamadapc> 2013
API_SHORTEN_URL="http://api.bitly.com/v3/shorten"

API_KEY=""
LOGIN=""

POST_STR="$API_SHORTEN_URL?apiKey=$API_KEY&login=$LOGIN&longUrl=$1"

function main() {
  curl -s "$POST_STR" 2>&1|
  grep -o '"url": [^ ,]*' | grep -o 'http:.*"' | sed 's/[\\"]//g'
}

main
