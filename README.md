emq_auth_clientid
=================

ClientId Authentication Plugin

Build
-----

```
make && make tests
```

Configuration
-------------

etc/emq_auth_clientid.conf:

```
auth.clientid.$clientid=$password

auth.clientid.$clientid@$ipaddr=$password
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

feng at emqtt.io

