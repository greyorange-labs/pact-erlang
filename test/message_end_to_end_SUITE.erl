-module(message_end_to_end_SUITE).

-compile(nowarn_export_all).
-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() -> [{group, consumer}, {group, producer}].

groups() ->
    [
        {consumer, [animal_consume_message_1, animal_consume_message_2, animal_consume_message_3, animal_consume_message_4]},
        {producer, [verify_producer]}
    ].


init_per_suite(Config) ->
    inets:start(),
    pact:enable_logging(<<"./pact_erlang.log">>, trace),
    Config.

end_per_suite(_Config) ->
    inets:stop(),
    ok.

init_per_group(consumer, Config) ->
    PactRef = pact:v4(<<"animal_service">>, <<"weather_service">>),
    [{pact_ref, PactRef} | Config];
init_per_group(producer, Config) ->
    Config;
init_per_group(_, Config) ->
    Config.

end_per_group(consumer, Config) ->
    PactRef = ?config(pact_ref, Config),
    pact:cleanup(PactRef),
    ok;
end_per_group(producer, _Config) ->
    ok;
end_per_group(_, _Config) ->
    ok.


animal_consume_message_1(Config) ->
    PactRef = ?config(pact_ref, Config),
    Message = pact:like(#{
        weather => #{
            temperature => 23.0,
            humidity => 75.5,
            wind_speed_kmh => 29
        },
        timestamp => <<"2024-03-14T10:22:13+05:30">>
    }),
    TestMessage = pact:msg_interaction(PactRef,
    #{
        given => <<"weather data for animals">>,
        upon_receiving => <<"a weather data message">>,
        with_contents => Message
    }),
    #{<<"contents">> := TestMessageContents} = TestMessage,
    ?assertMatch(ok, animal_service:process_weather_data(TestMessageContents)),
    pact:write(PactRef).

animal_consume_message_2(Config) ->
    PactRef = ?config(pact_ref, Config),
    Message = pact:like(#{
        weather => #{
            temperature => 23.0,
            humidity => 75.5,
            wind_speed_kmh => 29
        },
        timestamp => <<"2024-03-14T10:22:13+05:30">>
    }),
    TestMessage = pact:msg_interaction(PactRef,
    #{
        given => #{
            state => <<"weather data for animals 2">>
        },
        upon_receiving => <<"a weather data message 2">>,
        with_contents => Message
    }),
    #{<<"contents">> := TestMessageContents} = TestMessage,
    ?assertMatch(ok, animal_service:process_weather_data(TestMessageContents)),
    pact:write(PactRef).

animal_consume_message_3(Config) ->
    PactRef = ?config(pact_ref, Config),
    Message = pact:like(#{
        weather => #{
            temperature => 23.0,
            humidity => 75.5,
            wind_speed_kmh => 29
        },
        timestamp => <<"2024-03-14T10:22:13+05:30">>
    }),
    TestMessage = pact:msg_interaction(PactRef,
    #{
        given => #{
            state => <<"weather data for animals 3">>,
            params => #{
                <<"weather">> => <<"cold">>
            }
        },
        upon_receiving => <<"a weather data message 3">>,
        with_contents => Message
    }),
    #{<<"contents">> := TestMessageContents} = TestMessage,
    ?assertMatch(ok, animal_service:process_weather_data(TestMessageContents)),
    pact:write(PactRef).

animal_consume_message_4(Config) ->
    PactRef = ?config(pact_ref, Config),
    Message = pact:like(#{
        weather => #{
            temperature => 23.0,
            humidity => 75.5,
            wind_speed_kmh => 29
        },
        timestamp => <<"2024-03-14T10:22:13+05:30">>
    }),
    TestMessage = pact:msg_interaction(PactRef,
    #{
        upon_receiving => <<"a weather data message 4">>,
        with_contents => Message
    }),
    #{<<"contents">> := TestMessageContents} = TestMessage,
    ?assertMatch(ok, animal_service:process_weather_data(TestMessageContents)),
    pact:write(PactRef).

verify_producer(_Config) ->
    {ok, Cwd} = file:get_cwd(),
    PactDirectory = Cwd ++ "/pacts",
    {0, _} = pact_broker_client:publish_pacts(list_to_binary(PactDirectory)),
    Name = <<"weather_service">>,
    Version =  <<"default">>,
    Scheme = <<"http">>,
    Host = <<"localhost">>,
    Path = <<"/message_pact/verify">>,
    Branch = <<"develop">>,
    FilePath = <<"./pacts">>,
    WrongFilePath = <<"./pactss">>,
    BrokerUrl = <<"http://localhost:9292/">>,
    WrongBrokerUrl = <<"http://localhost:8282/">>,
    Protocol = <<"message">>,
    BrokerConfigs = #{
        broker_url => BrokerUrl,
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
        base_url => Path,
        branch => Branch,
        pact_source_opts => BrokerConfigs,
        message_providers => #{
            <<"a weather data message">> => {weather_service, generate_message, [23.5, 20, 75.0]}
        },
        fallback_message_provider => {weather_service, generate_message, [24.5, 20, 93.0]},
        protocol => Protocol
    },
    {ok, VerfierRef} = pact_verifier:start_verifier(Name, ProviderOpts),
    Output = pact_verifier:verify(VerfierRef),
    ProviderOpts1 = ProviderOpts#{pact_source_opts => #{file_path => FilePath}},
    {ok, VerifierRef1} = pact_verifier:start_verifier(Name, ProviderOpts1),
    Output1 = pact_verifier:verify(VerifierRef1),
    ProviderOpts2 = ProviderOpts#{pact_source_opts => #{file_path => WrongFilePath}},
    {ok, VerifierRef2} = pact_verifier:start_verifier(Name, ProviderOpts2),
    Output2 = pact_verifier:verify(VerifierRef2),
    ProviderOpts3 = ProviderOpts#{pact_source_opts => maps:update(broker_url, WrongBrokerUrl, BrokerConfigs)},
    {ok, VerifierRef3} = pact_verifier:start_verifier(Name, ProviderOpts3),
    {Output3, _, _} = pact_verifier:verify_v2(VerifierRef3),
    ?assertEqual(0, Output1),
    ?assertEqual(0, Output),
    ?assertEqual(1, Output2),
    ?assertEqual(1, Output3).



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
