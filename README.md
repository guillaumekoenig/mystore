hardlink to save files ?
write this readme

Add mystore.nix's full path to the imports of configuration.nix,
then activate the service :

```
services.mystore.enable = true;
services.mystore.folder = "/home/mystore";
services.mystore.user = "mystore";
users.users.mystore = { createHome = true; home = "/home/mystore"; };
```
