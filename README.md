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

[REST API](https://developer.emqx.io/docs/emq/v3/en/rest.html)
------------

List all clientids:
```
# Request
GET api/v3/auth_clientid

# Response
{
    "code": 0,
    "data": ["username1"]
}
```

Add cliendid:
```
# Request
POST api/v3/auth_clientid
{
    "clientid": "some_name",
    "password": "password"
}

# Response
{
    "code": 0
}
```

Update password for a clientid:

```
# Request
PUT api/v3/auth_clientid/$CLIENTID

{
    "password": "password"
}

# Response
{
    "code": 0
}

```

Lookup a clientid info:
```
# Request
GET api/v3/auth_clientid/$CLIENTID

# Response
{
    "code": 0,
    "data": {
        "clientid": "clientid",
        "password": "hash_password" 
    }
}
```

Delete a clientid:

```
# Request
DELETE api/v3/auth_clientid/$CLIENTID

# Response
{
    "code": 0
}

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

