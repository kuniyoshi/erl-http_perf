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
        stop ->
            ok
    end.

load(Url, RequestCount, ProcessCount) ->
    [?MODULE ! {get, Url, RequestCount} || _ <- lists:seq(1, ProcessCount)].

get(_Url, 0) ->
    done;
get(Url, Count) ->
    Start = now(),
    httpc:request(get,
                  {Url, []},
                  [{timeout, ?WAIT_MS}],
                  [{sync, false}, {receiver, fun ({Id, {Status, _Headers, _Body}}) ->
                                                     End = now(),
                                                     Took = element(2, End) - element(2, Start) + (element(3, End) - element(3, Start)) / 1000000,
                                                     io:format("ok - ~p ~p # ~p~n", [Id, Status, Took])
                                             end}]),
    get(Url, Count - 1).
