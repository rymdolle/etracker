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

-module(etracker).

-export([start/0, stop/0]).
-export([info/0]).
-export([announce/6, scrape/1]).

start() ->
    application:ensure_all_started(etracker).

stop() ->
    application:stop(etracker).

info() ->
    Hashes = etracker_torrent:list_all(),
    {ok, Scrape} = scrape(Hashes),
    lists:zip(Hashes, Scrape).

announce(InfoHash, Id, Address, Port, Event, Left) ->
    etracker_torrent:announce(InfoHash, Id, Address, Port, Event, Left).

scrape(Hashes) ->
    etracker_torrent:scrape(Hashes).
