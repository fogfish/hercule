%% @doc
%%
-module(hercule_bucket).
-behaviour(pipe).
-compile({parse_transform, category}).

-export([
   start_link/2,
   init/1,
   free/2,
   pipe/3
]).


-record(state, {
   sock = undefined :: _
}).


start_link(Ns, Bucket) ->
   pipe:start_link({via, pns, {urn, Ns, Bucket}}, ?MODULE, [Bucket], []).

init([{Bucket, _}]) ->
   {ok, Sock} = esio:socket(uri:segments([Bucket], hercule_config:storage())),
   {ok, pipe, #state{sock = Sock}}.

free(_, #state{sock = Sock}) ->
   esio:close(Sock).

%%
%%
pipe({get, {_, _, Key}}, Pipe, #state{sock = Sock} = State) ->
   {reply, esio:get(Sock, Key), pipe, State};

pipe({put, _, JsonLD}, Pipe, #state{sock = Sock} = State) ->
   {reply, elasticlog:append(Sock, JsonLD), pipe, State}.
