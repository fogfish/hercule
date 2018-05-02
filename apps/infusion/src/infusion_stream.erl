-module(infusion_stream).
-behavior(pipe).
-compile({parse_transform, category}).

-export([
   start_link/2,
   init/1,
   free/2,
   stream/3
]).

-record(state, {
   q = undefined :: _
}).


start_link(Ns, Uid) ->
   pipe:start_link(?MODULE, [Ns, Uid], []).

init([Ns, Uid]) ->
   [either ||
      pns:register(Ns, Uid, self()),
      Queue <- queue(Uid),
      cats:unit(stream, 
         #state{
            q = Queue
         }
      )
   ].

free(_, #state{q = Queue}) ->
   esq:free(Queue).

stream({put, _, Fact}, Pipe, #state{q = Queue} = State) ->
   pipe:ack(Pipe, esq:enq(Fact, Queue)),
   {next_state, stream, State};

stream({commit, Bucket}, Pipe, #state{q = Queue} = State) ->
   commit(Queue, Bucket),
   {stop, normal, State}.

%%
%%
queue(Uid) ->
   esq:new(
      scalar:c(filename:join([opts:val(streams, infusion), Uid])),
      [{tts, 5000}, {capacity, 1024}]
   ).

%%
%%
stream(Queue) ->
   stream:unfold(fun unfold/1, Queue).

unfold(Queue) ->
   case esq:deq(128, Queue) of
      [] ->
         undefined;
      Chunk ->
         Head = scalar:s([X || #{payload := X} <- Chunk]),
         {Head, Queue}
   end.

facts(Stream) ->
   [identity ||
      semantic:nt(Stream),
      stream:map(fun semantic:typed/1, _),
      semantic:fold(_)
   ].

%%
%%
commit(Queue, Bucket) ->
   Uri = uri:segments([Bucket], uri:new(opts:val(storage, infusion))),
   {ok, Sock} = esio:socket(Uri, []),
   stream:foreach(
      fun(Fact) -> 
         {ok, _} = elasticlog:append(Sock, Fact) 
      end, 
      facts( stream(Queue) )
   ).






