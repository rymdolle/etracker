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

-module(etracker_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).


%%% API

start(_StartType, _StartArgs) ->
    Routes = [{"/announce", etracker_http, []},
              {"/scrape", etracker_http, []}],
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    Port = application:get_env(etracker, port, 6969),
    cowboy:start_clear(etracker_http, [{port, Port}],
                       #{env => #{dispatch => Dispatch}}),
    etracker_sup:start_link().

stop(_State) ->
    ok.


%%% Internal functions
