Pact Erlang
=====

![Build Status](https://github.com/greyorange-labs/pact_erlang/actions/workflows/erlang.yml/badge.svg?event=push)
[![Code Coverage](https://codecov.io/gh/greyorange-labs/pact_erlang/branch/develop/graph/badge.svg?token=9F8XCB1TBR)](https://codecov.io/gh/greyorange-labs/pact_erlang)
[![Hex Version](https://img.shields.io/hexpm/v/pact_erlang.svg)](https://hex.pm/packages/pact_erlang)
[![Hex Docs](https://img.shields.io/badge/hex-docs-lightgreen.svg)](https://hexdocs.pm/pact_erlang/)
[![Total Download](https://img.shields.io/hexpm/dt/pact_erlang.svg)](https://hex.pm/packages/pact_erlang)
[![License](https://img.shields.io/hexpm/l/pact_erlang.svg)](https://github.com/greyorange-labs/pact_erlang/blob/develop/LICENSE)


An erlang library for contract testing using pact ffi and generating consumer pacts.

This library should be considered alpha quality. It is not yet feature complete and the API is subject to change.

Docs: https://hexdocs.pm/pact_erlang \
Changelog: https://hexdocs.pm/pact_erlang/changelog.html

Build
-----

    $ make


Add pact-erlang as a dependency in your application
---------------------------------------------------
```
{deps, [pact_erlang]}.
```

Usage
-----

Check the test folder for an example end-to-end scenario including consumer and producer checks.

```erlang
%% Setup
%% Define pact consumer and producer
PactRef = pact:v4(<<"consumer">>, <<"producer">>).

%% Define the interaction, returns running mock server port
{ok, Port} = pact:interaction(PactRef,
    #{
        given => #{
            state => <<"a user ranjan exists">>
        },
        upon_receiving => <<"get all users">>,
        with_request => #{
            method => <<"GET">>,
            path => <<"/users">>
        },
        will_respond_with => #{
            status => 200,
            headers => #{
                <<"Content-Type">> => <<"application/json">>
            },
            body => #{users => [#{user_id => 1, user_name => <<"ranjan">>, age => 26}]}
        }
    }).


%% test your code which calls the api
Users = user:get_users(<<"127.0.0.1">>, Port).


%% Verify if everything matched successfully
assertEqual({ok, matched}, pact:verify(PactRef)).

%% Should write pact file if matched, creates a new folder `pacts'
%% and writes the pact file inside it.
pact:write(PactRef).

%% Alternatively, one can override the default pacts directory path
pact:write(PactRef, "/path/to/pacts").

%% Cleanup test setup
%% This won't cleanup the pact files, only the pact ref you created in the test setup
pact:cleanup(PactRef).
```

Message pacts usage
-------------------

```erlang

PactRef = pact:v4(<<"animal_service">>, <<"weather_service">>),

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

```


Pact verification
-----------------

```erlang

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
Output = pact_verifier:verify(VerfierRef).
%% or you can use the verify_v2/1 which returns logs of file and url based %% pacts
{Output, OutputLog1, OutputLog2} = pact_verifier:verify_v2(VerfierRef)

```


Matching request path and request/response headers, and body values
-----

```erlang
%% Alternatively, you can also match things inside each request/response
pact:interaction(PactRef,
    #{
        upon_receiving => <<"a request to create an animal: Lazgo">>,
        with_request => #{
            method => <<"POST">>,
            path => <<"/animals">>,
            headers => #{
                <<"Content-Type">> => <<"application/json">>
            },
            body => #{
                <<"name">> => pact:like(<<"Lazgo">>),
                <<"type">> => pact:like(<<"dog">>)
            }
        },
        will_respond_with => #{
            status => 201
        }
    })
```

Release Checklist
-----

- Update version in `src/pact_erlang.app.src`
- Update CHANGELOG.md
- Run `rebar3 hex publish --dry-run` and make sure there are no un-intended files in included files
- Commit files, add a git tag matching the new version, and push to remote
- Run `rebar3 hex publish` to publish
