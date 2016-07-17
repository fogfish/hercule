%% @doc
%%
-module(hercule_fact).

-export([
   allowed_methods/1,
   content_provided/1, 
   content_accepted/1,
   'POST'/3
]).

%%
allowed_methods(_Req) ->
   ['POST'].

%%
content_provided(_Req) ->
   [{application, json}].

%%
content_accepted(_Req) ->
   [{application, json}].


'POST'({application, json}, JsonLD, {_Url, _Head, Env}) ->
   Ns  = lens:get(lens:pair(<<"ns">>), Env),
   Uri = uri:new( opts:val(storage, hercule) ),
   RDF = rdf( jsonld(JsonLD) ),
   {ok, Sock} = esio:socket(uri:segments([scalar:s(Ns)], Uri)),
   case lens:get(lens:pair(<<"action">>, <<"put">>), Env) of
      <<"remove">> ->
         ok = elasticnt:remove(Sock, RDF);
      _ ->
         ok = elasticnt:put(Sock, RDF)
   end,
   esio:close(Sock),
   ok.

%%
%% decode binary to json-ld
jsonld(JsonLD) ->
   assert( decode(JsonLD) ).

decode(JsonLD) ->
   case jsx:decode(JsonLD, [return_maps]) of
      Json when is_list(Json) ->
         Json;
      Json ->
         [Json]
   end.

assert(List) ->
   lists:filter(
      fun(X) -> maps:is_key(<<"@id">>, X) end,
      List
   ).

%%
%% decode json-ld to nt collections
rdf(JsonLD) ->
   lists:flatten(
      lists:map(
         fun(X) -> semantic:typed(semantic:jsonld(X)) end,
         JsonLD
      )
   ).
