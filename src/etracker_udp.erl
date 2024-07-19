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

-module(etracker_udp).

-behaviour(gen_server).

-export([port/0]).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {socket}).

-define(UDP_PROTOCOL_ID, 16#41727101980).

-define(UDP_CONNECT,  0).
-define(UDP_ANNOUNCE, 1).
-define(UDP_SCRAPE,   2).
-define(UDP_ERROR,    3).

-record(etracker_udp, {
            cid     :: integer(),
            timer   :: reference(),
            address :: inet:ip_address(),
            port    :: inet:port_number()
        }).


port() ->
    gen_server:call(?MODULE, port).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ets:new(etracker_udp, [named_table, set, {keypos, #etracker_udp.cid}]),
    Port = application:get_env(etracker, port, 6969),
    case gen_udp:open(Port, [binary, {active, true}]) of
        {ok, Socket} ->
            {ok, #state{socket = Socket}};
        {error, Reason} ->
            {stop, {error, Reason}}
    end.

handle_call(port, _From, State) ->
    {ok, {_, Port}} = inet:sockname(State#state.socket),
    {reply, Port, State}.

handle_cast(_Message, State) ->
    {stop, no_cast, State}.

handle_info({udp, Socket, FromAddress, FromPort,
             <<?UDP_PROTOCOL_ID:64, ?UDP_CONNECT:32, Tid:32>>}, State) ->
    <<Cid:64>> = crypto:strong_rand_bytes(8),
    TRef = erlang:start_timer(timer:minutes(2), self(), Cid),
    Connection = #etracker_udp{cid=Cid,
                               timer=TRef,
                               address=FromAddress,
                               port=FromPort},
    ets:insert(etracker_udp, Connection),
    Reply = <<?UDP_CONNECT:32, Tid:32, Cid:64>>,
    ok = gen_udp:send(Socket, FromAddress, FromPort, Reply),
    {noreply, State};
handle_info({udp, Socket, FromAddress, FromPort,
             <<Cid:64, Action:32, Tid:32, Data/binary>>}, State) ->
    case ets:lookup(etracker_udp, Cid) of
        [#etracker_udp{address=FromAddress, port=FromPort}] when Action == ?UDP_SCRAPE ->
            Hashes = [InfoHash || <<InfoHash:20/bytes>> <= Data],
            {ok, Scrape} = etracker_torrent:scrape(Hashes),
            Reply = << ?UDP_SCRAPE:32, Tid:32,
                       << <<Complete:32, Downloads:32, Incomplete:32>> ||
                           #{complete := Complete,
                             downloads := Downloads,
                             incomplete := Incomplete} <- Scrape >>/bytes >>,
            ok = gen_udp:send(Socket, FromAddress, FromPort, Reply),
            {noreply, State};
        [#etracker_udp{address=FromAddress, port=FromPort}] when Action == ?UDP_ANNOUNCE ->
            <<InfoHash:20/bytes, Id:20/bytes,
              _RX:64, Left:64, _TX:64, Event:32,
              Ip:32, _Key:32, _NumWant:32, Port:16, _Extensions/bytes>> = Data,
            {ok, Address} = address(Ip, FromAddress),
            {ok, #{interval := Interval, peers := Peers}} =
                etracker_torrent:announce(InfoHash, Id, Address, Port, event(Event), Left),
            EncodedPeers = << << (encode_peer(A, P))/bytes>>
                              || {_, A, P} <- Peers, tuple_size(A) == tuple_size(FromAddress) >>,
            {ok, [#{complete := Complete,
                    incomplete := Incomplete}]} = etracker_torrent:scrape([InfoHash]),
            Reply = << ?UDP_ANNOUNCE:32, Tid:32, Interval:32, Incomplete:32, Complete:32,
                       EncodedPeers/bytes>>,
            ok = gen_udp:send(Socket, FromAddress, FromPort, Reply),
            {noreply, State};
        _ ->
            {noreply, State}
    end;
handle_info({udp, _Socket, _FromAddress, _FromPort, _Data}, State) ->
    {noreply, State};
handle_info({timeout, TRef, Cid}, State) ->
    ets:match_delete(etracker_udp, #etracker_udp{cid=Cid, timer=TRef, _='_'}),
    {noreply, State}.


%%% Internal functions

address(0, FromAddress) ->
    {ok, FromAddress};
address(Address, _FromAddress) ->
    inet:parse_address(Address).

encode_peer(Address, Port) ->
    << << <<N:(tuple_size(Address) * 2)>> || N <- tuple_to_list(Address)>>/bytes, Port:16>>.

event(0) -> none;
event(1) -> completed;
event(2) -> started;
event(3) -> stopped.
