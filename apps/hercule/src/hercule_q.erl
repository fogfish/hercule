-module(hercule_q).

-export([
   allowed_methods/1,
   content_provided/1, 
   content_accepted/1,
   'GET'/2,
   'PUT'/3
]).

%%
allowed_methods(_Req) ->
   ['GET', 'PUT'].

%%
content_provided(_Req) ->
   [{application, json}].

%%
content_accepted(_Req) ->
   %% @todo: support diff content types
   [{application, json}].
   % [{text, datalog}].

%%
%%
'GET'(_, {Url, _Head, Env}) ->
   Uri = uri:new( opts:val(storage, hercule) ),
   Ns  = lens:get(lens:pair(<<"ns">>), Env),
   Id  = lens:get(lens:pair(<<"id">>), Env),
   Heap= lists:foldl(
      fun({Key, Val}, Acc) -> 
         Acc#{scalar:atom(Key) => scalar:decode(Val)} 
      end, 
      #{}, 
      uri:q(Url)
   ),
   [{_, Fn}] = ets:lookup(hercule, Id),
   {ok, Sock} = esio:socket(uri:segments([Ns], Uri)),
   Stream = datalog:q(Fn, Heap, Sock),
   {200, jsx:encode( stream:list(Stream) )}.
   
%%
%%
'PUT'(_, {_Url, _Head, Env}, Datalog) ->
   Id = lens:get(lens:pair(<<"id">>), Env),
   Fn = elasticlog:c( scalar:c(Datalog) ),
   ets:insert(hercule, {Id, Fn}),
   ok.

