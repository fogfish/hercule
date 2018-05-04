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
      Sock <- esio:socket( uri:segments([Uid], uri:new(opts:val(storage, hercule))) , []),
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
deduct({deduct, Datalog}, Pipe, #state{sock = Sock} = State) ->
   try
      Query = elasticlog:c( elasticlog:p( scalar:c( erlang:iolist_to_binary(Datalog) ) ) ),
      List  = stream:list(100, datalog:q(Query, Sock) ),
      Json  = [encode(X) || X <- List],
      pipe:ack(Pipe, {ok, Json})
   catch _:{case_clause, _Reason} ->
      pipe:ack(Pipe, {error, not_found})
   end,
   {next_state, deduct, State};

deduct({entity, Key}, Pipe, #state{sock = Sock} = State) ->
   pipe:ack(Pipe, esio:get(Sock, unique_key(Key))),
   {next_state, deduct, State};

deduct({fact, Fact}, Pipe, #state{sock = Sock} = State) ->
   pipe:ack(Pipe, elasticlog:append(Sock, Fact)),
   {next_state, deduct, State}.

%%
%%
encode(Map) ->
   [{Key, iri_val(Val)} || {Key, Val} <- maps:to_list(Map)].

iri_val({iri, Prefix, Suffix}) ->
   <<Prefix/binary, $:, Suffix/binary>>;
iri_val(Val) ->
   Val.

%%
%%
unique_key(S) ->
   base64( crypto:hash(md5, [<<(erlang:phash2(S)):32>>]) ).

base64(Hash) ->
   << << (urlencode(D)) >> || <<D>> <= base64:encode(Hash), D =/= $= >>.

urlencode($/) -> $_;
urlencode($+) -> $-;
urlencode(D)  -> D.



