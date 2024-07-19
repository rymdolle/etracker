%%% Copyright (c) 2024 Olle Mattsson <rymdolle@gmail.com>
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.

-module(etracker_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0]).

-export([suite/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_group/2, end_per_group/2]).

-export([announce/1]).
-export([scrape/1]).
-export([downloads/1]).
-export([peers/1]).

suite() ->
    [].

all() ->
    [{group, announce},
     {group, scrape}].

groups() ->
    [{announce, [], [announce, peers]},
     {scrape, [], [scrape, downloads]}].

init_per_suite(Config) ->
    application:set_env(etracker, port, 0),
    application:set_env(etracker, interval, 60),
    etracker:start(),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

announce(_Config) ->
    InfoHash = <<0:160>>,
    Addr = {127,0,2,1},
    {ok, #{peers := []}} = etracker:announce(InfoHash, <<1:160>>, Addr, 6880, started, 1),
    {ok, #{peers := [{<<1:160>>, Addr, 6880}]}} =
        etracker:announce(InfoHash, <<2:160>>, Addr, 6881, started, 1),
    {ok, #{peers := [{<<2:160>>, Addr, 6881}]}} =
        etracker:announce(InfoHash, <<1:160>>, Addr, 6880, stopped, 1),
    {ok, #{peers := []}} = etracker:announce(InfoHash, <<2:160>>, Addr, 6881, stopped, 1),
    ok.

peers(_Config) ->
    InfoHash = <<0:160>>,
    Count = 10,
    Peers = start_peers(Count, InfoHash),
    Expected = [{Id, Addr, Port} || #{id := Id, addr := Addr, port := Port} <- Peers],
    {ok, #{peers := Result}} =
        etracker:announce(InfoHash, <<1:160>>, {127,0,0,1}, 6880, started, 1),
    true = lists:all(fun(Peer) ->
                             lists:member(Peer, Expected)
                     end, Result),
    Count = length(Result),
    etracker:announce(InfoHash, <<1:160>>, {127,0,0,1}, 6880, stopped, 1),
    ok = stop_peers(Peers, InfoHash).

scrape(_Config) ->
    InfoHash = <<0:160>>,
    Count = 10,
    {ok, [#{incomplete := Complete}]} = etracker:scrape([InfoHash]),
    Peers = start_peers(Count, InfoHash),
    Expected = Complete + Count,
    {ok, [#{incomplete := Expected}]} = etracker:scrape([InfoHash]),
    ok = stop_peers(Peers, InfoHash),
    {ok, [#{incomplete := Complete}]} = etracker:scrape([InfoHash]).

downloads(_Config) ->
    InfoHash = <<0:160>>,
    Count = 4,
    {ok, [#{downloads := Downloads}]} = etracker:scrape([InfoHash]),
    Peers1 = start_peers(Count, InfoHash),
    Peers2 = complete_peers(Peers1, InfoHash),
    ok = stop_peers(Peers2, InfoHash),
    Expected = Downloads + Count,
    {ok, [#{downloads := Expected}]} = etracker:scrape([InfoHash]).


start_peers(0, _InfoHash) -> [];
start_peers(N, InfoHash) ->
    Id = base64:encode(crypto:strong_rand_bytes(15)),
    Addr = {127,0,2,1},
    Port = 6880 + N,
    Left = rand:uniform(100),
    {ok, _} = etracker:announce(InfoHash, Id, Addr, Port, started, Left),
    [#{id => Id,
       addr => Addr,
       port => Port,
       left => Left}|start_peers(N-1, InfoHash)].

stop_peers([], _InfoHash) -> ok;
stop_peers([#{id := Id, addr := Addr, port := Port, left := Left}|Rest], InfoHash) ->
    {ok, _} = etracker:announce(InfoHash, Id, Addr, Port, stopped, Left),
    stop_peers(Rest, InfoHash).

complete_peers([], _InfoHash) -> [];
complete_peers([Peer|Rest], InfoHash) ->
    Id = maps:get(id, Peer),
    Addr = maps:get(addr, Peer),
    Port = maps:get(port, Peer),
    _Left = maps:get(left, Peer),
    {ok, _} = etracker:announce(InfoHash, Id, Addr, Port, completed, 0),
    [Peer#{left => 0}|complete_peers(Rest, InfoHash)].
