-module(pact_consumer).

-export([
    v4/2,
    interaction/2,
    msg_interaction/2,
    verify_interaction/1,
    write_interaction/2,
    cleanup/1,
    encode_value/1
]).

-type consumer() :: binary().
-type provider() :: binary().
-type pact_pid() :: pid().
-type pact_interaction_details() :: map().
-type pact_mock_server_port() :: integer().
-type pact_message_data() :: map().

-spec v4(consumer(), provider()) -> pact_pid().
v4(Consumer, Provider) ->
    pact_consumer_http:v4(Consumer, Provider).

-spec interaction(pact_pid(), pact_interaction_details()) ->
    {ok, pact_mock_server_port()}.
interaction(PactPid, Interaction) ->
    pact_consumer_http:interaction(PactPid, Interaction).

-spec msg_interaction(pact_pid(), pact_interaction_details()) ->
    pact_message_data().
msg_interaction(PactPid, Interaction) ->
    pact_consumer_msg:interaction(PactPid, Interaction).

-spec verify_interaction(pact_pid()) -> {ok, matched} | {error, not_matched}.
verify_interaction(PactPid) ->
    MockServerPort = pact_ref_server:get_mock_server_port(PactPid),
    pactffi_nif:mock_server_matched(MockServerPort).

-spec write_interaction(pact_pid(), binary()) -> ok.
write_interaction(PactPid, Path) ->
    PactRef = pact_ref_server:get_pact_ref(PactPid),
    pactffi_nif:pact_handle_write_file(PactRef, Path, 0),
    pact_consumer_http:cleanup_interaction(PactPid).

-spec cleanup(pact_pid()) -> ok.
cleanup(PactPid) ->
    pact_consumer_http:cleanup_interaction(PactPid),
    pact_ref_server:stop(PactPid).

%% Internal Functions

-spec encode_value(map() | binary()) -> binary().
encode_value(Value) ->
    %% Checking if someone used regex_match
    case is_map(Value) of
        true ->
            thoas:encode(Value);
        false ->
            Value
    end.
