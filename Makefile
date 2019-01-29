PROJECT = emqx_auth_clientid
PROJECT_DESCRIPTION = EMQ X Authentication with ClientId/Password
PROJECT_VERSION = 3.1

DEPS = emqx_passwd clique
dep_emqx_passwd = git-emqx https://github.com/emqx/emqx-passwd v1.0
dep_clique      = git-emqx https://github.com/emqx/clique v0.3.11

BUILD_DEPS = emqx cuttlefish emqx_management
dep_emqx = git-emqx https://github.com/emqx/emqx release-3.1
dep_cuttlefish = git-emqx https://github.com/emqx/cuttlefish v2.2.1
dep_emqx_management = git-emqx https://github.com/emqx/emqx-management release-3.1

NO_AUTOPATCH = cuttlefish

ERLC_OPTS += +debug_info

TEST_ERLC_OPTS += +debug_info

COVER = true

$(shell [ -f erlang.mk ] || curl -s -o erlang.mk https://raw.githubusercontent.com/emqx/erlmk/master/erlang.mk)

include erlang.mk

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_auth_clientid.conf -i priv/emqx_auth_clientid.schema -d data

