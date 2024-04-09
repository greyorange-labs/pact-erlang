-module(message_pact_SUITE).

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
    {ok, Port, HttpdPid} = pact_provider_verifier:start(0),
    Name = <<"weather_service">>,
    Version =  <<"default">>,
    Scheme = <<"http">>,
    Host = <<"localhost">>,
    Path = <<"/message_pact/verify">>,
    Branch = <<"develop">>,
    FilePath = <<"./pacts">>,
    Protocol = <<"message">>,
    Output = pactffi_nif:verify_file_pacts(Name, Scheme, Host, Port, Path, Version, Branch, FilePath, Protocol, self(), <<"">>),
    ?assertEqual(0, Output),
    pact_provider_verifier:stop(HttpdPid).
