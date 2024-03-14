-module(pact_consumer_http).

-export([
    v4/2,
    interaction/2,
    cleanup_interaction/1
]).

-type pact_ref() :: integer().
-type pact_interaction_ref() :: integer().
-type consumer() :: binary().
-type provider() :: binary().
-type pact_pid() :: pid().
-type pact_interaction_details() :: map().
-type pact_mock_server_port() :: integer().
-type request_details() :: map().
-type response_details() :: map().

-spec v4(consumer(), provider()) -> pact_pid().
v4(Consumer, Provider) ->
    {ok, PactPid} = pact_ref_server:start(Consumer, Provider),
    PactPid.

-spec interaction(pact_pid(), pact_interaction_details()) ->
    {ok, pact_mock_server_port()}.
interaction(PactPid, Interaction) ->
    {PactRef, InteractionRef} = init_interaction(PactPid, Interaction),
    RequestDetails = maps:get(with_request, Interaction, #{}),
    ok = insert_request_details(InteractionRef, RequestDetails),
    ResponseDetails = maps:get(will_respond_with, Interaction, #{}),
    ok = insert_response_details(InteractionRef, ResponseDetails),
    start_mock_server(PactPid, PactRef, <<"127.0.0.1">>, 0, <<"http">>).

-spec cleanup_interaction(pact_pid()) -> ok.
cleanup_interaction(PactPid) ->
    PactRef = pact_ref_server:get_pact_ref(PactPid),
    MockServerPort = pact_ref_server:get_mock_server_port(PactPid),
    case MockServerPort of
        undefined ->
            ok;
        _ ->
            ok = pactffi_nif:cleanup_mock_server(MockServerPort)
    end,
    pactffi_nif:free_pact_handle(PactRef).

%% Internal functions

-spec init_interaction(pact_pid(), pact_interaction_details()) ->
    {pact_ref(), pact_interaction_ref()}.
init_interaction(PactPid, Interaction) ->
    {Consumer, Producer} = pact_ref_server:get_consumer_producer(PactPid),
    PactRef = pactffi_nif:new_pact(Consumer, Producer),
    ok = pact_ref_server:set_pact_ref(PactPid, PactRef),
    InteractionDesc = maps:get(upon_receiving, Interaction, <<"">>),
    InteractionRef = pactffi_nif:new_interaction(PactRef, InteractionDesc),
    case maps:get(given, Interaction, undefined) of
        undefined ->
            ok;
        GivenState when is_binary(GivenState) ->
            pactffi_nif:given(InteractionRef, GivenState);
        GivenStateWithParams when is_map(GivenStateWithParams) ->
            ProviderState = maps:get(state, GivenStateWithParams, <<"">>),
            StateJson = maps:get(params, GivenStateWithParams, undefined),
            case StateJson of
                undefined ->
                    pactffi_nif:given(InteractionRef, ProviderState);
                _ ->
                    NewStateJson = pact_consumer:encode_value(StateJson),
                    pactffi_nif:given_with_params(InteractionRef, ProviderState, NewStateJson)
            end
    end,
    ok = pact_ref_server:create_interaction(PactPid, InteractionRef, Interaction),
    {PactRef, InteractionRef}.

-spec start_mock_server(pact_pid(), pact_ref(), binary(), integer(), binary()) ->
    {ok, pact_mock_server_port()}.
start_mock_server(PactPid, PactRef, Host, Port, InteractionPart) ->
    MockServerPort = pactffi_nif:create_mock_server_for_transport(
        PactRef, Host, Port, InteractionPart
    ),
    ok = pact_ref_server:set_mock_server_port(PactPid, MockServerPort),
    {ok, MockServerPort}.

%% Internal functions

-spec insert_request_details(pact_interaction_ref(), request_details()) -> ok.
insert_request_details(InteractionRef, RequestDetails) ->
    ReqMethod = maps:get(method, RequestDetails),
    ReqPath = maps:get(path, RequestDetails),
    NewReqPath = pact_consumer:encode_value(ReqPath),
    pactffi_nif:with_request(InteractionRef, ReqMethod, NewReqPath),
    ReqHeaders = maps:get(headers, RequestDetails, #{}),
    ContentType = get_content_type(ReqHeaders),
    maps:fold(
        fun(Key, Value, _Acc) ->
            %% FIXME: 4th parameter is Index.. need to increment
            NewValue = pact_consumer:encode_value(Value),
            pactffi_nif:with_header_v2(InteractionRef, 0, Key, 0, NewValue)
        end,
        ok,
        ReqHeaders
    ),
    ReqBody = maps:get(body, RequestDetails, undefined),
    case ReqBody of
        undefined ->
            ok;
        _ ->
            NewReqBody = pact_consumer:encode_value(ReqBody),
            pactffi_nif:with_body(InteractionRef, 0, ContentType, NewReqBody)
    end,
    ReqQueryParams = maps:get(query_params, RequestDetails, undefined),
    case ReqQueryParams of
        undefined ->
            ok;
        _ ->
            maps:fold(
                fun(Key, Value, _Acc) ->
                    %% FIXME: 3rd parameter is Index.. need to increment
                    NewValue = pact_consumer:encode_value(Value),
                    pactffi_nif:with_query_parameter_v2(InteractionRef, Key, 0, NewValue)
                end,
                ok,
                ReqQueryParams
            )
    end,
    ok.

-spec insert_response_details(pact_interaction_ref(), response_details()) -> ok.
insert_response_details(InteractionRef, ResponseDetails) ->
    ResponseStatusCode = maps:get(status, ResponseDetails, undefined),
    case ResponseStatusCode of
        undefined -> ok;
        _ -> pactffi_nif:response_status(InteractionRef, ResponseStatusCode)
    end,
    ResHeaders = maps:get(headers, ResponseDetails, #{}),
    ContentType = get_content_type(ResHeaders),
    maps:fold(
        fun(Key, Value, _Acc) ->
            %% FIXME: 4th parameter is Index.. need to increment
            NewValue = pact_consumer:encode_value(Value),
            pactffi_nif:with_header_v2(InteractionRef, 1, Key, 0, NewValue)
        end,
        ok,
        ResHeaders
    ),
    ResBody = maps:get(body, ResponseDetails, undefined),
    case ResBody of
        undefined ->
            ok;
        _ ->
            NewResBody = pact_consumer:encode_value(ResBody),
            pactffi_nif:with_body(InteractionRef, 1, ContentType, NewResBody)
    end,
    ok.

-spec get_content_type(map()) -> binary().
get_content_type(Headers) ->
    maps:get(<<"Content-Type">>, Headers, <<"application/json">>).
