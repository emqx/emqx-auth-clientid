emq_auth_clientid
=================

Authentication with ClientId and Password

Build
-----

```
make && make tests
```

Configuration
-------------

etc/emq_auth_clientid.conf:

```
##auth.client.$N.clientid = clientid
##auth.client.$N.password = passwd
```

Load the Plugin
---------------

```
./bin/emqttd_ctl plugins load emq_auth_clientid
```

License
-------

Apache License Version 2.0

Author
------

Feng at emqtt.io

