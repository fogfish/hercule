-module(hercule_poirot).
-behaviour(pipe).
-compile({parse_transform, category}).

-export([
   schema/0,
   deduct/1
]).
-export([
   start_link/0,
   init/1,
   free/2,
   deduct/3
]).

-define(DEFAULT, "http://localhost:9200/nt").

-record(state, {
   sock = undefined :: _
}).


schema() ->
   pipe:call(?MODULE, schema, infinity).

deduct(Datalog) ->
   pipe:call(?MODULE, {deduct, Datalog}, infinity).


start_link() ->
   pipe:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
   [either || 
      cats:unit(#state{}),
      connect(?DEFAULT, _),
      cats:unit(deduct, _)
   ].

free(_, State) ->
   release(State).

%%
%%
deduct(schema, Pipe, #state{sock = Sock} = State) ->
   pipe:ack(Pipe, [either || elasticlog:schema(Sock), cats:unit(schema(_))]),
   {next_state, deduct, State};

deduct({deduct, Datalog}, Pipe, #state{sock = Sock} = State) ->
   io:format("==> ~p~n", [Datalog]),
   Query = elasticlog:c( elasticlog:p( scalar:c( erlang:iolist_to_binary(Datalog) ) ) ),
   io:format("==> ~p~n", [Query]),
   List  = stream:list( datalog:q(Query, Sock) ),
   io:format("==> ~p~n", [List]),
   Json  = [encode(X) || X <- List],
   io:format("==> ~p~n", [Json]),
   pipe:ack(Pipe, {ok, Json}),
   {next_state, deduct, State}.


%%
%%
connect(Uri, #state{} = State) ->
   [either ||
      esio:socket(Uri, []),
      cats:unit(State#state{sock = _})
   ].

%%
%%
release(#state{sock = Sock} = State) ->
   [either ||
      esio:close(Sock),
      cats:unit(State#state{sock = undefined})
   ].

%%
%%
schema(Schema) ->
   [#{key => iri(P), type => iri(Type)} || {P, Type} <- Schema].

iri({iri, Prefix, Suffix}) ->
   <<Prefix/binary, $:, Suffix/binary>>;
iri(undefined) ->
   <<$s>>.

encode(Map) ->
   [{Key, iri_val(Val)} || {Key, Val} <- maps:to_list(Map)].

iri_val({iri, Prefix, Suffix}) ->
   <<Prefix/binary, $:, Suffix/binary>>;
iri_val(Val) ->
   Val.

