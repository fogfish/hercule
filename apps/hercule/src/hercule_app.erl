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
   config(),
   hercule_sup:start_link(). 

%%
%%
stop(_State) ->
   ok.

%%
%%
config() ->
   {ok, Schema} = esio:socket( opts:val(storage, hercule) ),
   lists:foreach(
      fun(X) ->
         elasticnt:schema(Schema, X, [])
      end,
      opts:val(ns, hercule)
   ),
   esio:close(Schema).


