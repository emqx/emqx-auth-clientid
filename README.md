emqx_auth_clientid
==================

Authentication with ClientId and Password

Build
-----

```
make && make tests
```

Configuration
-------------

etc/emqx_auth_clientid.conf:

```
##auth.client.$N.clientid = clientid
##auth.client.$N.password = passwd
```

Load the Plugin
---------------

```
./bin/emqx_ctl plugins load emqx_auth_clientid
```

License
-------

Apache License Version 2.0

Author
------

EMQ X-Men Team.

