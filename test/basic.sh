#!/usr/bin/env bash

server=http://localhost:8081

get() {
    curl -s -o /dev/null -w %{http_code} $server$1
}

put() {
    curl -s -o /dev/null -w %{http_code} \
	 -X PUT \
	 -H 'Content-Type: application/octet-stream' \
	 --data-binary @$1 $server$2
}

delete() {
    curl -s -o /dev/null -w %{http_code} \
	 -X DELETE $server$1
}

assert_eq() {
    echo -n $1 $2...\ 
    r=$(eval $1)
    [ $r -eq $2 ] && echo "ok" || echo "FAIL"
}

assert_eq "get /basic.sh" 500	# fixme
assert_eq "put $0 /basic.sh" 200
# todo check content
assert_eq "delete /basic.sh" 200
assert_eq "delete /basic.sh" 500 # fixme
assert_eq "get /basic.sh" 500 	# fixme
