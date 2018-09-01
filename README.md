# Mystore

A simple REST service to store and retrieve files over http. The three
http verbs GET, PUT, DELETE are supported.

## Run as a service

A module is provided to deploy mystore on a NixOS machine. It is
implemented as a systemd service.

To get started, import `mystore.nix`'s full path in the machine's
`configuration.nix` system configuration file :

```
{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ...
      /path/to/mystore.nix
    ];
```

Mystore is then available as a service and can be activated as such on
the machine :

```
services.mystore.enable = true;
services.mystore.folder = "/home/mystore";
services.mystore.user = "mystore";
users.users.mystore = { createHome = true; home = "/home/mystore"; };
```

Check the module for configuration options. There are three options :
folder to serve, user to run mystore under, and port to serve requests
on (with default value 8081).

Usual systemd commands apply to inspect the status of mystore :

```
$ systemctl status mystore
● mystore.service - mystore service
   Loaded: loaded (/nix/store/72k07rzd2pcgiykfdl2lf8jfh3y5a0jm-unit-mystore.service/mystore.service; enabled; vendor preset: enabled)
   Active: active (running) since Sat 2018-09-01 23:03:43 CEST; 11s ago
 Main PID: 32254 (mystore)
    Tasks: 1 (limit: 4915)
   CGroup: /system.slice/mystore.service
           └─32254 /nix/store/d8pv9r7fkk2rdhf2xhhv0wxd4z3gl1wb-mystore-0.1.0.0/bin/mystore --port 8081 --folder /home/mystore

Sep 01 23:03:43 nixos systemd[1]: Started mystore service.
Sep 01 23:03:47 nixos mystore[32254]: Serving "/home/mystore" on :8081
Sep 01 23:03:47 nixos mystore[32254]: 127.0.0.1 - - [01/Sep/2018:23:03:47 +0200] "GET /basic.sh HTTP/1.1" 404 - "" "curl/7.59.0"
Sep 01 23:03:47 nixos mystore[32254]: 127.0.0.1 - - [01/Sep/2018:23:03:47 +0200] "PUT /basic.sh HTTP/1.1" 200 - "" "curl/7.59.0"
Sep 01 23:03:47 nixos mystore[32254]: 127.0.0.1 - - [01/Sep/2018:23:03:47 +0200] "GET /basic.sh HTTP/1.1" 200 - "" "curl/7.59.0"
Sep 01 23:03:47 nixos mystore[32254]: 127.0.0.1 - - [01/Sep/2018:23:03:47 +0200] "GET /basic.sh HTTP/1.1" 200 - "" "curl/7.59.0"
Sep 01 23:03:47 nixos mystore[32254]: 127.0.0.1 - - [01/Sep/2018:23:03:47 +0200] "DELETE /basic.sh HTTP/1.1" 200 - "" "curl/7.59.0"
```

As can be seen in the output, requests are logged by systemd (also
available via `journalctl -u mystore`).

## Run standalone

Mystore can be run standalone for testing :

```$ mystore --help
Mystore

Usage: mystore --port INT --folder STRING

Available options:
  -h,--help                Show this help text
  --port INT               Port to serve requests on
  --folder STRING          Folder to serve
```

## API

### GET /:name
Retrieve a file by name.

HTTP return codes :
- 200 : OK, file returned in response body
- 404 : file does not exist
- 500 : general backend error

### PUT /:name
Create or update a file given by name. File contents are provided in
the request body. The Content-Type http header field must be provided
and set to application/octet-stream.

HTTP return codes :
- 200 : OK, file was successfully saved
- 500 : general backend error

### DELETE /:name
Delete a file by name.

HTTP return codes :
- 200 : OK, file was removed
- 404 : file does not exist
- 500 : general backend error

## Testing

There is testing of the API in `test/basic.sh`. It assumes an empty
mystore instance is running on port 8081. The script will try to
upload itself to the service, and check that retrieval and removal are
working as expected.
