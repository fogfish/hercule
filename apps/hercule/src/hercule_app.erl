%%
%%   Copyright (c) 2016, Dmitry Kolesnikov
%%   All Rights Reserved.
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
-module(hercule_app).
-behaviour(application).

-export([
   start/2
  ,stop/1
]).

%%
%%
start(_Type, _Args) ->
   config(),
   hercule_sup:start_link(). 

%%
%%
stop(_State) ->
   ok.


%%
%%
config() ->
   datalog_db(),
   config_ns_schema( opts:val(ns, hercule) ),
   config_ns_schema( opts_env() ).


%%
%%
config_ns_schema(List) ->
   {ok, Schema} = esio:socket( storage() ),
   lists:foreach(
      fun(X) ->
         elasticnt:schema(Schema, X, [])
      end,
      List
   ),
   esio:close(Schema).


%%
%%
storage() ->
   os:getenv("HERCULE_STORAGE", opts:val(storage, hercule)).

%%
%%
opts_env() ->
   N = scalar:i( os:getenv("HERCULE_NS", 0) ),
   lists:map(
      fun(X) ->
          os:getenv("HERCULE_NS_" ++ scalar:c(X))
      end,
      lists:seq(1, N)
   ).

%%
%% create local storage for datalog statements
datalog_db() ->
   ets:new(hercule, [set, public, named_table]).

