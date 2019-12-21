-define(APP, emqx_auth_clientid).

-record(auth_metrics, {
        success = 'client.auth.success',
        failure = 'client.auth.failure',
        ignore = 'client.auth.ignore'
    }).

-define(AUTH_METRICS, tl(tuple_to_list(#auth_metrics{}))).
-define(AUTH_METRIC(K), #auth_metrics{}#auth_metrics.K).
