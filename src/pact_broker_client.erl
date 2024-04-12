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
    ct:pal("Cmd is ~p", [Cmd]),
    {RetCode, Output} = run_cmd(Cmd),
    ct:print("===> Publish Output: ~n~s", [Output]),
    {RetCode, Output}.

-spec run_cmd(string()) -> {integer(), string()}.
run_cmd(Command) ->
    Port = erlang:open_port({spawn, Command}, [stream, in, eof, hide, exit_status]),
    get_data_from_executable(Port, []).

get_data_from_executable(Port, Sofar) ->
    receive
        {Port, {data, Bytes}} ->
            get_data_from_executable(Port, [Sofar | Bytes]);
        {Port, eof} ->
            Port ! {self(), close},
            receive
                {Port, closed} -> true
            end,
            receive
                {'EXIT', Port, _} -> ok
            after
                % force context switch
                1 -> ok
            end,
            ExitCode =
                receive
                    {Port, {exit_status, Code}} ->
                        Code
                end,
            {ExitCode, lists:flatten(Sofar)}
    end.
