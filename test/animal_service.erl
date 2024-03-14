-module(animal_service).
-include_lib("inets/include/httpd.hrl").

-export([
    start/0,
    start/1,
    stop/1,
    do/1,
    process_weather_data/1
]).

-define(TABLE_NAME, animals).

start() ->
    start(8080).

start(Port) ->
    {ok, _} = application:ensure_all_started(inets),
    create_table(),
    {ok, Pid} = inets:start(httpd, [{bind_address, "127.0.0.1"}, {port, Port}, {server_name, "animal_service"}, {server_root, "./"}, {document_root, "./"}, {modules, [animal_service]}]),
    Info = httpd:info(Pid),
    {port, ListenPort} = lists:keyfind(port, 1, Info),
    {ok, ListenPort, Pid}.

stop(Pid) ->
    catch ets:delete(?TABLE_NAME),
    inets:stop(httpd, Pid).

create_table() ->
    ets:new(?TABLE_NAME, [set, public, named_table]).

insert_animal(Name, Type) when Name =/= undefined andalso Type =/= undefined ->
    ets:insert(?TABLE_NAME, {Name, Type});
insert_animal(_Name, _Type) -> {false, "Either name or type is missing"}.

reset_data() ->
    ets:delete_all_objects(?TABLE_NAME).

find_animal_by_name(Name) ->
    case ets:match_object(?TABLE_NAME, {Name, '_'}) of
        [] ->
            {error, not_found};
        [{Name, Type}] ->
            {ok, #{name => Name, type => Type}}
    end.

find_animals_by_type(Type) ->
    AnimalList = ets:match_object(?TABLE_NAME, {'_', Type}),
    lists:map(fun({N, T}) -> #{name => N, type => T} end, AnimalList).

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
        ["animals", Name] ->
            NameBinary = erlang:list_to_binary(Name),
            case find_animal_by_name(NameBinary) of
                {ok, Animal} ->
                    make_json_response(200, Animal);
                {error, not_found} ->
                    make_404_response()
            end;
        ["animals"] ->
            QueryStr = maps:get(query, UriMap, ""),
            Query = maps:from_list(uri_string:dissect_query(QueryStr)),
            case maps:get("type", Query, undefined) of
                undefined ->
                    make_json_response(400, #{error => <<"Missing query parameter: type">>});
                Type ->
                    TypeBinary = erlang:list_to_binary(Type),
                    AnimalObjectList = find_animals_by_type(TypeBinary),
                    make_json_response(200, #{animals => AnimalObjectList})
            end;
        _ ->
            make_404_response()
    end;
process_data(#mod{request_uri = "/animals", method = "POST", entity_body = Body}) ->
    case thoas:decode(Body) of
        {ok, Animal} ->
            Name = maps:get(<<"name">>, Animal, undefined),
            Type = maps:get(<<"type">>, Animal, undefined),
            case insert_animal(Name, Type) of
                true ->
                    make_json_response(201, #{ok => true});
                {false, Reason} ->
                    make_json_response(400, #{error => erlang:list_to_binary(Reason)})
            end;
        {error, Reason} ->
            io:format("JSON Decode Error: ~p~n", [Reason]),
            make_json_response(400, #{error => <<"Invalid JSON">>})
    end;
process_data(#mod{request_uri = "/pactStateChange", method = "POST", entity_body = Body}) ->
    {ok, StateRequest} = thoas:decode(Body),
    RequiredState = maps:get(<<"state">>, StateRequest, <<"">>),
    case RequiredState of
        <<"">> -> reset_data();
        <<"an alligator with the name Mary exists">> ->
            case maps:get(<<"params">>, StateRequest, undefined) of
                undefined ->
                    insert_animal(<<"Mary">>, <<"alligator">>);
                Params ->
                    Name = maps:get(<<"name">>, Params, <<"">>),
                    Type = maps:get(<<"type">>, Params, <<"">>),
                    insert_animal(Name, Type)
            end;
        <<"a dog with the name Duke exists">> ->
            insert_animal(<<"Duke">>, <<"Dog">>)
    end,
    make_json_response(200, #{ok => true});
process_data(_ModData) ->
    make_404_response().

make_json_response(Code, Body) ->
    BodyJson = erlang:binary_to_list(thoas:encode(Body)),
    Length = io_lib:format("~w", [io_lib:chars_length(BodyJson)]),
    {proceed, [{response, {response, [{code, Code}, {content_length, Length}, {content_type, "application/json"}], BodyJson}}]}.

make_404_response() ->
    make_json_response(404, #{error => not_found}).

process_weather_data(Payload) ->
    #{
        <<"weather">> := #{
            <<"temperature">> := _Temp,
            <<"humidity">> := _Humidity,
            <<"wind_speed_kmh">> := _WindSpeed
        },
        <<"timestamp">> := _TimeStamp
    } = Payload,
    %% Do something with weather data like validation,
    %% Db update
    ok.
