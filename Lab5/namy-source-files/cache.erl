-module(cache).

-export([lookup/2, add/4, remove/2, purge/1]).

lookup(Name, Cache) ->
    case lists:keyfind(Name, 1, Cache) of
        {Name, Reply, Expire} ->
            Now = erlang:convert_time_unit(
                      erlang:monotonic_time(), native, second),
            if Expire > Now ->
                   Reply;
               true ->
                   invalid
            end;
        false ->
            unknown
    end.

add(Name, Expire, Reply, Cache) ->
    lists:keystore(Name, 1, Cache, {Name, Reply, Expire}).

remove(Name, Cache) ->
    lists:keydelete(Name, 1, Cache).

purge(Cache) ->
    Now = erlang:convert_time_unit(erlang:monotonic_time(), native, second),
    lists:filter(fun({_, _, Expire}) -> Expire > Now end, Cache).