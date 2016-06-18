-module(hercule).

-export([start/0]).
-export([q/2,q/3]).

start() ->
   applib:boot(?MODULE, 
      code:where_is_file("dev.config")
   ).

%%
%% evaluates datalog query and return stream
-spec q(_, _) -> datum:stream().
-spec q(_, _, _) -> datum:stream().

q(Ns, Datalog) ->
   q(Ns, #{}, Datalog).

q(Ns, Heap, Datalog)
 when is_function(Datalog) ->
   Uri = uri:new( opts:val(storage, hercule) ),
   {ok, Sock} = esio:socket(uri:segments([scalar:s(Ns)], Uri)),
   datalog:q(Datalog, Heap, Sock);

q(Ns, Heap, Datalog) ->
   q(Ns, Heap, elasticlog:c(scalar:c(Datalog))).
