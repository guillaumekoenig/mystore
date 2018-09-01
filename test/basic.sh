#!/usr/bin/env bash

server=http://localhost:8081

get() {
    curl -s -o /dev/null -w "%{http_code}" "$server$1"
}

get_contents() {
    curl -s "$server$1"
}

put() {
    curl -s -o /dev/null -w "%{http_code}" \
	 -X PUT \
	 -H 'Content-Type: application/octet-stream' \
	 --data-binary @"$1" "$server$2"
}

delete() {
    curl -s -o /dev/null -w "%{http_code}" \
	 -X DELETE "$server$1"
}

globalret=0
assert_eq() {
    echo -n "$1" "$2"...' '
    r=$(eval "$1")
    # shellcheck disable=SC2015
    [[ $r -eq $2 ]] && echo "ok" || { echo "FAIL"; globalret=1; }
}

assert_contents() {
    echo -n "$1" == "$2"...' '
    # shellcheck disable=SC2015
    cmp <(eval "$1") <(eval "$2") && echo "ok" \
            || { echo "FAIL"; globalret=1; }
}

assert_eq "get /basic.sh" 404
assert_eq "put $0 /basic.sh" 200
assert_eq "get /basic.sh" 200
assert_contents "get_contents /basic.sh" "cat $0"
assert_eq "delete /basic.sh" 200
assert_eq "delete /basic.sh" 404
assert_eq "get /basic.sh" 404

exit $globalret
