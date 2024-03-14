-module(pact_ref_server).
-behaviour(gen_server).

-export([
    start/2,
    create_interaction/3,
    set_mock_server_port/2,
    get_mock_server_port/1,
    stop/1,
    get_pact_ref/1,
    set_pact_ref/2,
    get_consumer_producer/1
]).
-export([init/1, handle_call/3, terminate/2]).

-dialyzer(no_behaviours).

-type pact_ref() :: undefined | integer().
-type pact_interaction_ref() :: integer().
-type consumer() :: binary().
-type provider() :: binary().
-type pact_interaction_details() :: map().
-type pact_interaction() :: {pact_interaction_ref(), pact_interaction_details()}.
-type pact_mock_server_port() :: undefined | integer().

%% erlfmt-ignore
-record(pact_state, {
    consumer                        :: consumer(),
    producer                        :: provider(),
    pact_ref = undefined            :: undefined | pact_ref(),
    interaction = {undefined, #{}}  :: {undefined, #{}} | pact_interaction(),
    mock_server_port = undefined    :: undefined | pact_mock_server_port()
}).

%% @doc Starts pact server
-spec start(consumer(), provider()) -> gen_server:start_ret().
start(Consumer, Producer) ->
    gen_server:start(
        {global, {?MODULE, Consumer, Producer}},
        ?MODULE,
        #pact_state{
            consumer = Consumer,
            producer = Producer
        },
        []
    ).

%% @doc Gets consumer and producer for a pact ref
-spec get_consumer_producer(pid()) -> {consumer(), provider()}.
get_consumer_producer(PactPid) ->
    gen_server:call(PactPid, get_consumer_producer).

%% @doc Gets pact ref generated (when a new pact was created/re-created) by pact ffi
-spec get_pact_ref(pid()) -> pact_ref().
get_pact_ref(PactPid) ->
    gen_server:call(PactPid, get_pact_ref).

%% @doc Sets pact_ref in pact_handler's state
-spec set_pact_ref(pid(), pact_ref()) -> ok.
set_pact_ref(PactPid, PactRef) ->
    gen_server:call(PactPid, {set_pact_ref, PactRef}).

%% @doc Public API to create an interaction
-spec create_interaction(pid(), pact_interaction_ref(), pact_interaction_details()) -> ok.
create_interaction(PactPid, InteractionRef, Interaction) ->
    gen_server:call(PactPid, {create_interaction, InteractionRef, Interaction}).

%% @doc Public API to set the mock server port when the server is started
-spec set_mock_server_port(pid(), pact_mock_server_port()) -> ok.
set_mock_server_port(PactPid, Port) ->
    gen_server:call(PactPid, {set_mock_server_port, Port}).

%% @doc Public API to set the mock server port when the server is started
-spec get_mock_server_port(pid()) -> pact_mock_server_port().
get_mock_server_port(PactPid) ->
    gen_server:call(PactPid, get_mock_server_port).

%% @doc Stops the pact_handler gen_server for final cleanups
-spec stop(pid()) -> ok.
stop(PactPid) ->
    gen_server:stop(PactPid).

%% Gen_server callbacks

init(#pact_state{consumer = Consumer, producer = Producer}) ->
    {ok, #pact_state{consumer = Consumer, producer = Producer}}.

handle_call({create_interaction, InteractionRef, Interaction}, _From, State) ->
    NewState = State#pact_state{interaction = {InteractionRef, Interaction}},
    {reply, ok, NewState};
handle_call({set_mock_server_port, Port}, _From, State) ->
    NewState = State#pact_state{mock_server_port = Port},
    {reply, ok, NewState};
handle_call(get_mock_server_port, _From, State) ->
    {reply, State#pact_state.mock_server_port, State};
handle_call(get_pact_ref, _From, State) ->
    {reply, State#pact_state.pact_ref, State};
handle_call({set_pact_ref, PactRef}, _From, State) ->
    NewState = State#pact_state{pact_ref = PactRef},
    {reply, ok, NewState};
handle_call(get_consumer_producer, _From, State) ->
    {reply, {State#pact_state.consumer, State#pact_state.producer}, State}.

terminate(_Reason, _State) ->
    ok.
