-module(hercule_app).
-behaviour(application).

-export([
   start/2
  ,stop/1
]).

%%
%%
start(_Type, _Args) ->
   ets:new(hercule, [set, public, named_table]),
   hercule_sup:start_link(). 

%%
%%
stop(_State) ->
   ok.
