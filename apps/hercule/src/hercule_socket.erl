-module(hercule_socket).
-behaviour(pipe).

-export([
   start_link/2
,  init/1
,  free/2
,  handle/3
]).

start_link(Id, Host) ->
   pipe:start_link(?MODULE, [Id, Host], []).

init([Id, Host]) ->
   {ok, Sock} = esio:socket(Host, []),
   erlang:link(Sock),
   ok = pns:register(socket, Id, Sock),
   {ok, handle, Sock}.

free(_, Sock) ->
   esio:close(Sock).

handle(_, _, Sock) ->
   {next_state, handle, Sock}.