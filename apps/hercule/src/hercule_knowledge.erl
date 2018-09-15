-module(hercule_knowledge).
-behaviour(pipe).
-compile({parse_transform, category}).

-export([
   start_link/2,
   init/1,
   free/2,
   deduct/3
]).


-record(state, {
   sock = undefined :: _
}).


start_link(Ns, Uid) ->
   pipe:start_link(?MODULE, [Ns, Uid], []).

init([Ns, Uid]) ->
   [either || 
      pns:register(Ns, Uid, self()),
      Sock <- esio:socket( uri:segments([Uid], uri:new(opts:val(storage, hercule))) ),
      cats:unit(deduct,
         #state{
            sock = Sock
         }
      )
   ].

free(_, #state{sock = Sock}) ->
   esio:close(Sock).

%%
%%
deduct({deduct, User, N, Datalog}, Pipe, #state{sock = Sock} = State) ->
   try
      io:format(">>>>>>>>~n~s~n>>>>>>>>~n", [erlang:iolist_to_binary(Datalog)]),
      Script = datalog:p( scalar:c( erlang:iolist_to_binary(Datalog) ) ),
      Json = stream:list(N, 
         elasticlog:jsonify(
            datalog:schema(Script), 
            elasticlog:q(
               datalog:c(elasticlog, Script), 
               #{<<"username">> => User}, 
               Sock
            )
         )
      ),
      pipe:ack(Pipe, {ok, Json})
   catch _:{case_clause, _Reason} ->
      pipe:ack(Pipe, {error, not_found})
   end,
   {next_state, deduct, State};

deduct({entity, Key}, Pipe, #state{sock = Sock} = State) ->
   pipe:ack(Pipe, esio:get(Sock, Key)),
   {next_state, deduct, State};

deduct({fact, JsonLD}, Pipe, #state{sock = Sock} = State) ->
   pipe:ack(Pipe, elasticlog:append(Sock, JsonLD)),
   {next_state, deduct, State}.
