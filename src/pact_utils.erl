-module(pact_utils).

-export([
    run_executable_async/1
]).

-spec run_executable_async(string()) -> {integer(), string()}.
run_executable_async(Cmd) ->
    Port = erlang:open_port({spawn, Cmd}, [stream, in, eof, hide, exit_status]),
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
