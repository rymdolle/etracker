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

-module(etracker_http).

-behaviour(cowboy_handler).

-export([port/0]).
-export([init/2]).
-export([known_methods/2,
         allowed_methods/2,
         malformed_request/2,
         resource_exists/2,
         content_types_provided/2]).

-export([announce/2, scrape/2]).

port() ->
    ranch:get_port(etracker_http).

init(Req, _Options) ->
    {cowboy_rest, Req, #{}}.

known_methods(Req, State) ->
    {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>], Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>], Req, State}.

malformed_request(Req, State) ->
    {Func, Required} = case cowboy_req:path(Req) of
                           <<"/announce">> ->
                               {Address, _Port} = cowboy_req:peer(Req),
                               {announce, [{info_hash, fun hash/2},
                                           peer_id,
                                           {port, int},
                                           {uploaded, int},
                                           {downloaded, int},
                                           {left, int},
                                           {key, [], undefined},
                                           {ip, [fun address/2], Address},
                                           {numwant, [int], 50},
                                           {compact, [int], 1},
                                           {event, [fun event/2], none}
                                          ]};
                           <<"/scrape">> ->
                               {scrape, [{info_hash, fun hash/2}]}
                       end,
    Match = cowboy_req:match_qs(Required, Req),
    try {false, Req, Match#{function => Func}}
    catch _:_ ->
            Body = bencode:encode([{<<"failure reason">>, <<"invalid request">>}]),
            {true, cowboy_req:set_resp_body(Body, Req), State}
    end.

resource_exists(Req, State) ->
    {true, Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"text">>, <<"plain">>, '*'}, maps:get(function, State)}], Req, State}.

announce(Req, State = #{info_hash := [InfoHash]}) ->
    {ok, [#{complete := Complete,
            downloads := Downloads,
            incomplete := Incomplete}]} = etracker_torrent:scrape([InfoHash]),
    #{left := Left,
      compact := Compact,
      ip := Address,
      port := Port,
      event := Event,
      peer_id := Id} = State,
    {ok, #{interval := Interval, peers := Peers}} =
        etracker_torrent:announce(InfoHash, Id, Address, Port, Event, Left),
    Body = [{<<"complete">>, Complete},
            {<<"downloaded">>, Downloads},
            {<<"incomplete">>, Incomplete},
            {<<"interval">>, Interval},
            {<<"peers">>, encode_peers(Peers, Compact == 1)}],
    {bencode:encode(Body), Req, State}.

scrape(Req, State = #{info_hash := Hashes}) ->
    {ok, Result} = etracker_torrent:scrape(Hashes),
    Body = [{<<"files">>,
             [begin
                  {InfoHash,
                   [{<<"complete">>, Complete},
                    {<<"downloaded">>, Downloads},
                    {<<"incomplete">>, Incomplete}]}
              end || {InfoHash, {Complete, Downloads, Incomplete}} <- lists:zip(Hashes, Result)]}],
    {bencode:encode(Body), Req, State}.


%% Encoding

encode_peers(Peers, Compress) when Compress ->
    compress(Peers, <<>>);
encode_peers(Peers, Compress) when not Compress ->
    plain(Peers).

plain([]) -> [];
plain([{Id, Address, Port}|Rest]) ->
    [[{<<"peer id">>, Id},
      {<<"ip">>, list_to_binary(inet:ntoa(Address))},
      {<<"port">>, Port}] | plain(Rest)].

compress([], Acc) -> Acc;
compress([{_Id, Address, Port}|Rest], Acc) ->
    A = << <<N:(tuple_size(Address) * 2)>> || N <- tuple_to_list(Address)>>,
    compress(Rest, <<Acc/bytes, A/bytes, Port:16>>).


%% Constraints

hash(forward, <<InfoHash:20/bytes>>) ->
    {ok, [InfoHash]};
hash(forward, Hashes) when is_list(Hashes) ->
    io:format("~p\n", [Hashes]),
    {ok, [InfoHash || <<InfoHash:20/bytes>> <- Hashes]}.

address(forward, Bin) ->
    inet:parse_address(binary_to_list(Bin)).

event(forward, <<"started">>) ->
    {ok, started};
event(forward, <<"completed">>) ->
    {ok, completed};
event(forward, <<"stopped">>) ->
    {ok, stopped};
event(forward, _) ->
    {ok, none}.
