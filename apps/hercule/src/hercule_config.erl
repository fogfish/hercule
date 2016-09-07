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
%% @doc
%%   config deployment service
-module(hercule_config).
-behaviour(pipe).

-export([
   start_link/0,
   init/1,
   free/2,
   setup/3
]).


start_link() ->
   pipe:start_link(?MODULE, [], []).

init(_) ->
   erlang:send_after(1000, self(), timeout),
   {ok, setup, config()}.

free(_, _State) ->
   ok. 

setup(timeout, _, [Config | Tail] = State) ->
   lager:notice("[hercule] : config ~p~n", [Config]),
   {ok, Sock} = esio:socket( storage() ),
   case elasticnt:schema(Sock, Config, []) of
      ok ->
         erlang:send_after(0, self(), timeout),
         esio:close(Sock),
         {next_state, setup, Tail};

      % schema already deployed
      {error, 400} ->   
         erlang:send_after(0, self(), timeout),
         esio:close(Sock),
         {next_state, setup, Tail};

      {error, _  } ->
         erlang:send_after(5000, self(), timeout),
         esio:close(Sock),
         {next_state, setup, State}
   end;

setup(_, _, []) ->
   {next_state, setup, []}.

%%
%%
storage() ->
   os:getenv("HERCULE_STORAGE", opts:val(storage, hercule)).

%%
%%
config() ->
   config_sys() ++ config_env().

config_sys() ->
   opts:val(ns, [], hercule).

config_env() ->
   N = scalar:i( os:getenv("HERCULE_NS", 0) ),
   lists:map(
      fun(X) ->
          os:getenv("HERCULE_NS_" ++ scalar:c(X))
      end,
      lists:seq(1, N)
   ).

