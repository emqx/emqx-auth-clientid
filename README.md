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

## Password hash.
##
## Value: plain | md5 | sha | sha256
auth.client.password_hash = sha256
```

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

EMQ X Team.

