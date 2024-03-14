-module(pact_broker_client).

-export([
    publish_pacts/1
]).

-spec publish_pacts(binary()) -> {integer(), string()}.
publish_pacts(Directory) ->
    Cmd =
        "docker run --network host --rm --add-host=host.docker.internal:host-gateway -v " ++
            binary_to_list(Directory) ++
            ":/pacts "
            "-e PACT_DO_NOT_TRACK=true "
            "-e PACT_BROKER_BASE_URL=http://localhost:9292/ "
            "-e PACT_BROKER_USERNAME=pact_workshop "
            "-e PACT_BROKER_PASSWORD=pact_workshop "
            "pactfoundation/pact-cli:latest-multi publish /pacts "
            "--consumer-app-version default --branch default",
    {RetCode, Output} = run_cmd(Cmd),
    {RetCode, Output}.

-spec run_cmd(string()) -> {integer(), string()}.
run_cmd(Cmd) ->
    Res = os:cmd(Cmd ++ "\nRET_CODE=$?\necho \"\n$RET_CODE\""),
    [[], RetCode | Rest] = lists:reverse(string:split(Res, "\n", all)),
    Result = lists:join("\n", lists:reverse(Rest)),
    {list_to_integer(RetCode), Result}.
