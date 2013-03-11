-module(http_perf).
-export([start/0, stop/0, loop/0]).
-export([load/3, get/2]).
%-compile([export_all]).
-define(WAIT_MS, 3000).

start() ->
    register(?MODULE, spawn(?MODULE, loop, [])).

stop() ->
    ?MODULE ! stop,
    unregister(?MODULE).

loop() ->
    receive
        {get, URL, Count} ->
            spawn(?MODULE, get, [URL, Count]),
            loop();
        {took, Url, MicroSeconds, StatusCode, StatusMessage} ->
            io:format("~p[s] - ~p ~p GET ~p~n", [MicroSeconds, StatusCode, StatusMessage, Url]),
            loop();
        stop ->
            ok
    end.

load(Url, RequestCount, ProcessCount) ->
    [?MODULE ! {get, Url, RequestCount} || _ <- lists:seq(1, ProcessCount)].

get(_Url, 0) ->
    done;
get(Url, Count) ->
    {MicroSeconds, {ok, {{_Version, StatusCode, StatusMessage}, _Head, _Body}}}
        = timer:tc(httpc, request, [get,
                                    {Url, []},
                                    [{timeout, ?WAIT_MS}],
                                    []]),
%    io:format("~p ~p ~p ~p~n", [MicroSeconds, Url, StatusCode, StatusMessage]),
    ?MODULE ! {took, Url, MicroSeconds, StatusCode, StatusMessage},
    get(Url, Count - 1).
