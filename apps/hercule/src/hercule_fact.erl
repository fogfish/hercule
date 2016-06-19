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
   RDF = lists:flatten(
      lists:map(
         fun(X) -> semantic:typed(semantic:jsonld(X)) end,
         identity(jsonld(JsonLD))
      )
   ),
   {ok, Sock} = esio:socket(uri:segments([scalar:s(Ns)], Uri)),
   ok = elasticnt:in(Sock, RDF),
   esio:close(Sock),
   ok.

jsonld(JsonLD) ->
   case jsx:decode(JsonLD, [return_maps]) of
      Json when is_list(Json) ->
         Json;
      Json ->
         [Json]
   end.

identity(List) ->
   lists:map(
      fun(X) ->
         case maps:is_key(<<"@id">>, X) of
            false ->
               X#{<<"@id">> => bits:btoh(crypto:hash(sha, jsx:encode(X)))};
            true  ->
               X
         end
      end,
      List
   ).
