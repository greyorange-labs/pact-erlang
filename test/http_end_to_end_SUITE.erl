-module(http_end_to_end_SUITE).
-compile(nowarn_export_all).
-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() -> [{group, consumer},{group, producer}].

groups() ->
    [
        {consumer, [get_animal_success, get_animal_success_2, get_animal_failure, create_animal, search_animals]},
        {producer, [verify_producer]}
    ].

init_per_suite(Config) ->
    inets:start(),
    pact:enable_logging(trace),
    Config.

end_per_suite(_Config) ->
    inets:stop(),
    ok.

init_per_group(consumer, Config) ->
    PactRef = pact:v4(<<"myapp">>, <<"animal_service">>),
    [{pact_ref, PactRef} | Config];
init_per_group(producer, Config) ->
    Config.

end_per_group(consumer, Config) ->
    PactRef = ?config(pact_ref, Config),
    pact:cleanup(PactRef),
    ok;
end_per_group(producer, Config) ->
    Config.

get_animal_success(Config) ->
    PactRef = ?config(pact_ref, Config),
    AnimalObject = #{<<"name">> => <<"Mary">>, <<"type">> => <<"alligator">>},
    {ok, Port} = pact:interaction(PactRef,
    #{
        given => #{
            state => <<"an alligator with the name Mary exists">>,
            params => thoas:encode(AnimalObject)
        },
        upon_receiving => <<"a request to GET an animal: Mary">>,
        with_request => #{
            method => <<"GET">>,
            path => <<"/animals/Mary">>
        },
        will_respond_with => #{
            status => 200,
            headers => #{
                <<"Content-Type">> => <<"application/json">>
            },
            body => AnimalObject
        }
    }),
    ?assertMatch({ok, AnimalObject}, animal_service_interface:get_animal(Port, "Mary")),
    {ok, matched} = pact:verify(PactRef),
    pact:write(PactRef).

get_animal_success_2(Config) ->
    PactRef = ?config(pact_ref, Config),
    AnimalObject = #{<<"name">> => <<"Duke">>, <<"type">> => <<"Dog">>},
    {ok, Port} = pact:interaction(PactRef,
    #{
        given => <<"a dog with the name Duke exists">>,
        upon_receiving => <<"a request to GET an animal: Duke">>,
        with_request => #{
            method => <<"GET">>,
            path => <<"/animals/Duke">>
        },
        will_respond_with => #{
            status => 200,
            headers => #{
                <<"Content-Type">> => <<"application/json">>
            },
            body => thoas:encode(AnimalObject)
        }
    }),
    ?assertMatch({ok, AnimalObject}, animal_service_interface:get_animal(Port, "Duke")),
    {ok, matched} = pact:verify(PactRef),
    pact:write(PactRef).

get_animal_failure(Config) ->
    PactRef = ?config(pact_ref, Config),
    {ok, Port} = pact:interaction(PactRef,
    #{
        upon_receiving => <<"a request to GET a non-existing animal: Miles">>,
        with_request => #{
            method => <<"GET">>,
            path => pact:regex_match(<<"/animals/Miles">>, <<"^\/animals\/[a-zA-Z]+">>)
        },
        will_respond_with => #{
            status => 404,
            headers => #{
                <<"Content-Type">> => <<"application/json">>
            },
            body => #{error => not_found}
        }
    }),
    ?assertMatch({error, not_found}, animal_service_interface:get_animal(Port, "Miles")),
    {ok, matched} = pact:verify(PactRef),
    pact:write(PactRef).

create_animal(Config) ->
    PactRef = ?config(pact_ref, Config),
    AnimalPactObject = pact:like(#{
        <<"name">> => <<"Max">>,
        <<"type">> => <<"dog">>,
        <<"age">> => 3,
        <<"nickname">> => <<"koko">>,
        <<"weight_kg">> => 12.0,
        <<"gender">> => pact:regex_match(<<"male">>, <<"(male|female)">>),
        <<"carnivorous">> => true,
        <<"siblings">> => pact:each_like(<<"lola">>),
        <<"list_att">> => pact:each_like([1,pact:like(<<"head">>)]),
        <<"children">> => [<<"coco">>],
        <<"children_details">> => pact:each_like(#{<<"name">> => <<"coco">>, <<"age">> => 1, <<"body_size">> => [3,4,5]}),
        <<"attributes">> => pact:each_key(#{<<"happy">> => true}, <<"(happy|ferocious)">>)
    }),
    AnimalObject = [
        {<<"name">>, <<"Max">>},
        {<<"type">>, <<"dog">>},
        {<<"age">>, 3},
        {<<"nickname">>, <<"lazgo">>},
        {<<"weight_kg">>, 10.0},
        {<<"gender">>, <<"male">>},
        {<<"carnivorous">>, true},
        {<<"siblings">>, [<<"lola">>, <<"mary">>]},
        {<<"list_att">>, [[2, <<"legs">>]]},
        {<<"children">>,  [<<"coco">>]},
        {<<"children_details">>, [[{<<"name">>, <<"coco">>}, {<<"age">>, 1}, {<<"body_size">>, [3,4,5]}]]},
        {<<"attributes">>, [{<<"ferocious">>, false}]}
    ],
    {ok, Port} = pact:interaction(PactRef,
    #{
        upon_receiving => <<"a request to create an animal: Max">>,
        with_request => #{
            method => <<"POST">>,
            path => <<"/animals">>,
            headers => #{
                <<"Content-Type">> => <<"application/json">>
            },
            body => AnimalPactObject
        },
        will_respond_with => #{
            status => 201
        }
    }),
    ?assertMatch(ok, animal_service_interface:create_animal(Port, AnimalObject)),
    {ok, matched} = pact:verify(PactRef),
    pact:write(PactRef).

search_animals(Config) ->
    PactRef = ?config(pact_ref, Config),
    Result = #{<<"animals">> => [#{<<"name">> => <<"Mary">>, <<"type">> => <<"alligator">>}]},
    Query = #{<<"type">> => <<"alligator">>},
    {ok, Port} = pact:interaction(PactRef,
    #{
        given => #{
            state => <<"an alligator with the name Mary exists">>
        },
        upon_receiving => <<"a request to find all alligators">>,
        with_request => #{
            method => <<"GET">>,
            path => <<"/animals">>,
            query_params => Query
        },
        will_respond_with => #{
            headers => #{
                <<"Content-Type">> => <<"application/json">>
            },
            body => Result
        }
    }),
    ?assertMatch({ok, Result}, animal_service_interface:search_animals(Port, Query)),
    {ok, matched} = pact:verify(PactRef),
    pact:write(PactRef).

verify_producer(_Config) ->
    {ok, Cwd} = file:get_cwd(),
    PactDirectory = Cwd ++ "/pacts",
    {0, _} = pact_broker_client:publish_pacts(list_to_binary(PactDirectory)),
    {ok, Port, HttpdPid} = animal_service:start(8080),
    unlink(HttpdPid),
    Name = <<"animal_service">>,
    Version =  <<"default">>,
    Scheme = <<"http">>,
    Host = <<"localhost">>,
    Path = <<"/">>,
    Branch = <<"develop">>,
    FilePath = <<"./pacts">>,
    Protocol = <<"http">>,
    StateChangePath = list_to_binary("http://localhost:" ++ integer_to_list(Port) ++ "/pactStateChange"),
    BrokerOpts = #{
            broker_url => <<"http://localhost:9292/">>,
            broker_username => <<"pact_workshop">>,
            broker_password => <<"pact_workshop">>,
            enable_pending => 1,
            consumer_version_selectors => thoas:encode(#{})
        },
    ProviderOpts = #{
        name => Name,
        version => Version,
        scheme => Scheme,
        host => Host,
        port => Port,
        base_url => Path,
        branch => Branch,
        pact_source_opts => BrokerOpts,
        state_change_url => StateChangePath,
        % message_providers => #{
        %     <<"a weather data message">> => {message_pact_SUITE, generate_message, [23.5, 20, 75.0]}
        % },
        % fallback_message_provider => {message_pact_SUITE, generate_message, [24.5, 20, 93.0]},
        protocol => Protocol
    },
    {ok, VerifierRef} = pact_verifier:start_verifier(Name, ProviderOpts),
    Output = pact_verifier:verify(VerifierRef),
    ProviderOpts1 = ProviderOpts#{pact_source_opts => #{file_path => FilePath}},
    {ok, VerifierRef1} = pact_verifier:start_verifier(Name, ProviderOpts1),
    Output1 = pact_verifier:verify(VerifierRef1),
    ProviderOpts2 = ProviderOpts#{pact_source_opts => maps:put(file_path, FilePath, BrokerOpts)},
    {ok, VerifierRef2} = pact_verifier:start_verifier(Name, ProviderOpts2),
    Output2 = pact_verifier:verify(VerifierRef2),
    ?assertEqual(0, Output1),
    ?assertEqual(0, Output),
    ?assertEqual(0, Output2),
    animal_service:stop(HttpdPid).
