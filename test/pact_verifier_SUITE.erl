-module(pact_verifier_SUITE).
-compile(nowarn_export_all).
-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("inets/include/httpd.hrl").

all() -> [test_api_handler_failure].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

test_api_handler_failure(_Config) ->
    ModData = #mod{request_uri = "/message_pact/verify", method = "GET"},
    ?assertEqual([{response, {500, "Internal Server Error"}}], pact_verifier:do(ModData)).
