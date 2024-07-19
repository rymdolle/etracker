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

-module(etracker_torrent).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([announce/6]).
-export([scrape/1]).
-export([list_all/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

-record(state, {}).

-record(etracker_peer, {info_hash, id, key, addr, port, left, last}).

-define(TAB, etracker_peers).


%%% API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

announce(InfoHash, Id, Address, Port, Event, Left) ->
    Request = {announce, InfoHash, Id, Address, Port, Event, Left},
    gen_server:call(?MODULE, Request).

scrape(Hashes) when is_list(Hashes) ->
    gen_server:call(?MODULE, {scrape, Hashes}).

list_all() ->
    ets:foldl(fun(#etracker_peer{info_hash=InfoHash}, Acc) ->
                      case lists:member(InfoHash, Acc) of
                          true ->
                              Acc;
                          false ->
                              [InfoHash|Acc]
                      end
              end, [], ?TAB).


%%% gen_server callbacks

init([]) ->
    ets:new(?TAB, [named_table, bag, protected, {keypos, #etracker_peer.info_hash}]),
    {ok, #state{}}.


terminate(_Reason, #state{}) ->
    ok.

handle_call({scrape, Hashes}, _From, State) ->
    {reply, {ok, [begin
                      Match = #etracker_peer{info_hash=InfoHash, left='$0', last='$3', _='_'},

                      %% Delete old entries
                      delete_expired(InfoHash),

                      Complete    = ets:select_count(?TAB, [{Match, [{'==', '$0', 0}], [true]}]),
                      Incomplete  = ets:select_count(?TAB, [{Match, [{'/=', '$0', 0}], [true]}]),
                      Downloads = ets:lookup_element(etracker_downloads, InfoHash, 2, 0),
                      #{complete => Complete,
                        downloads => Downloads,
                        incomplete => Incomplete}
                  end || <<InfoHash:20/bytes>> <- Hashes]}, State};
handle_call({announce, InfoHash, Id, Addr, Port, Event, Left}, _From, State) ->
    Now = erlang:system_time(second),

    delete_expired(InfoHash),

    case find_peer(InfoHash, Id, Addr, Port) of
        Result when Event == stopped ->
            [ets:delete_object(?TAB, Peer) || Peer <- Result];
        [Peer] when Event == completed, Left == 0 ->
            ets:delete_object(?TAB, Peer),
            ets:insert(?TAB, Peer#etracker_peer{left=Left, last=Now}),
            ets:update_counter(etracker_downloads, InfoHash, 1, {InfoHash, 0});
        [Peer] ->
            ets:delete_object(?TAB, Peer),
            ets:insert(?TAB, Peer#etracker_peer{left=Left, last=Now});
        [] ->
            ets:insert(?TAB, #etracker_peer{
                                info_hash=InfoHash,
                                id=Id,
                                addr=Addr,
                                port=Port,
                                left=Left,
                                last=Now
                               })
    end,

    Interval = application:get_env(etracker, interval, 900),
    Peers = list_peers(InfoHash, Addr, Port),
    {reply, {ok, #{interval => Interval,
                   peers => Peers}}, State}.

handle_cast(_Message, State) ->
    {stop, no_cast, State}.

handle_info(_Info, State) ->
    {stop, no_info, State}.


%%% Internal functions

delete_expired(InfoHash) ->
    Interval = application:get_env(etracker, interval, 900),
    Now = erlang:system_time(second),

    Match = #etracker_peer{
               info_hash=InfoHash,
               last='$0',
               _='_'
              },
    Guards = [{'>', Now - (Interval + 120), '$0'}],

    ets:select_delete(?TAB, [{Match, Guards, [true]}]).

find_peer(InfoHash, Id, Addr, Port) ->
    Match = #etracker_peer{
               info_hash=InfoHash,
               id=Id,
               addr=Addr,
               port=Port,
               _='_'
              },
    ets:select(?TAB, [{Match, [], ['$_']}]).

list_peers(InfoHash, Addr, Port) ->
        Match = #etracker_peer{
               info_hash=InfoHash,
               id='$0',
               addr='$1',
               port='$2',
               last='$3',
               _='_'
              },

    Result = {{'$0', '$1', '$2'}},

    %% Exclude the asking peer from list
    Guards = [{'==', {'tuple_size', '$1'}, 4},
              {'orelse',
               {'/=', '$1', {const, Addr}},
               {'/=', '$2', {const, Port}}}],
    Peers = ets:select(?TAB, [{Match, Guards, [Result]}]),
    shuffle(Peers).

shuffle(List) ->
    {First, Last} = lists:split(length(List) div 2, List),
    zip(First, Last).

zip([H], []) -> [H];
zip([], [H]) -> [H];
zip([], []) -> [];
zip([H1|T1], [H2|T2]) ->
    [H2, H1 | zip(T1, T2)].
