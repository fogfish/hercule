-module(hercule_socket_sup).
-behaviour(supervisor).

-export([start_link/2, init/1]).

-define(CHILD(Type, I),            {I,  {I, start_link,   []}, permanent, 5000, Type, dynamic}).
-define(CHILD(Type, I, Args),      {I,  {I, start_link, Args}, permanent, 5000, Type, dynamic}).
-define(CHILD(Type, ID, I, Args),  {ID, {I, start_link, Args}, permanent, 5000, Type, dynamic}).

%%-----------------------------------------------------------------------------
%%
%% supervisor
%%
%%-----------------------------------------------------------------------------

start_link(Pool, Host) ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, [Pool, Host]).
   
init([Pool, Host]) ->   
   {ok,
      {
         {one_for_one, 40, 600},
         [socket(Id, Host) || Id <- lists:seq(1, Pool)]
      }
   }.

%%
%%
socket(Id, Host) ->
   ?CHILD(worker, Id, hercule_socket, [Id, Host]).
