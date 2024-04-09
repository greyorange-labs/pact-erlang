-module(pact_provider_verifier).
-include_lib("inets/include/httpd.hrl").
% -behaviour(gen_server).

%% Web Server API
-export([
    start/0,
    start/1,
    stop/1,
    do/1,
    generate_message/3
]).

% -export([init/1, handle_call/3, terminate/2]).

-dialyzer(no_behaviours).

start() ->
    start(8080).

start(Port) ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, Pid} = inets:start(httpd, [
        {bind_address, "127.0.0.1"},
        {port, Port},
        {server_name, "pact_provider_verifier"},
        {server_root, "./"},
        {document_root, "./"},
        {modules, [pact_provider_verifier]}
    ]),
    Info = httpd:info(Pid),
    {port, ListenPort} = lists:keyfind(port, 1, Info),
    {ok, ListenPort, Pid}.

stop(Pid) ->
    inets:stop(httpd, Pid).

do(ModData) ->
    case catch process_data(ModData) of
        {'EXIT', Reason} ->
            io:format("Error: ~p~n", [Reason]),
            [{response, {500, "Internal Server Error"}}];
        Response ->
            Response
    end.

process_data(#mod{request_uri = ReqUri, method = "GET"}) ->
    UriMap = uri_string:parse(ReqUri),
    Path = maps:get(path, UriMap),
    SplitPath = string:tokens(Path, "/"),
    case SplitPath of
        ["test"] ->
            make_json_response(200, #{<<"hello">> => <<"moto">>});
        _ ->
            make_404_response()
    end;
process_data(#mod{request_uri = "/message_pact/verify", method = "POST", entity_body = Body}) ->
    case thoas:decode(Body) of
        {ok, StateReq} ->
            Description = maps:get(<<"description">>, StateReq, <<"">>),
            ArgsList = given_args_mapping(Description),
            Message = erlang:apply(pact_provider_verifier, generate_message, ArgsList),
            make_json_response(200, Message);
        {error, Reason} ->
            io:format("JSON Decode Error: ~p~n", [Reason]),
            make_json_response(400, #{error => <<"Invalid JSON">>})
    end.

make_json_response(Code, Body) ->
    BodyJson = erlang:binary_to_list(thoas:encode(Body)),
    Length = io_lib:format("~w", [io_lib:chars_length(BodyJson)]),
    {proceed, [
        {response,
            {response, [{code, Code}, {content_length, Length}, {content_type, "application/json"}],
                BodyJson}}
    ]}.

make_404_response() ->
    make_json_response(404, #{error => not_found}).

generate_message(Temperature, WindSpeed, Humidity) ->
    #{
        <<"weather">> => #{
            <<"temperature">> => Temperature,
            <<"humidity">> => Humidity,
            <<"wind_speed_kmh">> => WindSpeed
        },
        <<"timestamp">> => list_to_binary(
            calendar:system_time_to_rfc3339(erlang:system_time(second))
        )
    }.

given_args_mapping(Given) ->
    case Given of
        <<"">> -> [23.5, 20, 75.0];
        _ -> [24.5, 20, 93.0]
    end.

% %% Web Server End



% -type pact_ref() :: undefined | integer().
% -type version() :: binary().
% -type scheme() :: binary().
% -type provider() :: binary().
% -type host() :: binary().
% -type path() :: binary().
% -type branch() :: binary().
% -type file
% -type pact_interaction_details() :: map().
% -type pact_interaction() :: {pact_interaction_ref(), pact_interaction_details()}.
% -type pact_mock_server_port() :: undefined | integer().


% % Name = <<"animal_service">>,
% % Version =  <<"default">>,
% % Scheme = <<"http">>,
% % Host = <<"localhost">>,
% % Path = <<"/">>,
% % Branch = <<"develop">>,
% % FilePath = <<"./pacts">>,
% % Protocol = <<"http">>,
% % StateChangePath = list_to_binary("http://localhost:" ++ integer_to_list(Port) ++ "/pactStateChange"),

% %% erlfmt-ignore
% -record(pact_state, {
%     producer                        :: provider(),
%     pact_ref = undefined            :: undefined | pact_ref(),
%     interaction = {undefined, #{}}  :: {undefined, #{}} | pact_interaction(),
%     mock_server_port = undefined    :: undefined | pact_mock_server_port()
% }).

% %% @doc Starts pact server
% -spec start(consumer(), provider()) -> gen_server:start_ret().
% start(Consumer, Producer) ->
%     gen_server:start(
%         {global, {?MODULE, Consumer, Producer}},
%         ?MODULE,
%         #pact_state{
%             consumer = Consumer,
%             producer = Producer
%         },
%         []
%     ).




% %% Gen_server callbacks

% init(#pact_state{consumer = Consumer, producer = Producer}) ->
%     {ok, #pact_state{consumer = Consumer, producer = Producer}}.

% handle_call({create_interaction, InteractionRef, Interaction}, _From, State) ->
%     NewState = State#pact_state{interaction = {InteractionRef, Interaction}},
%     {reply, ok, NewState};
% handle_call({set_mock_server_port, Port}, _From, State) ->
%     NewState = State#pact_state{mock_server_port = Port},
%     {reply, ok, NewState};
% handle_call(get_mock_server_port, _From, State) ->
%     {reply, State#pact_state.mock_server_port, State};
% handle_call(get_pact_ref, _From, State) ->
%     {reply, State#pact_state.pact_ref, State};
% handle_call({set_pact_ref, PactRef}, _From, State) ->
%     NewState = State#pact_state{pact_ref = PactRef},
%     {reply, ok, NewState};
% handle_call(get_consumer_producer, _From, State) ->
%     {reply, {State#pact_state.consumer, State#pact_state.producer}, State}.

% terminate(_Reason, _State) ->
%     ok.
