%%
%%   Copyright (c) 2016, Dmitry Kolesnikov
%%   All Rights Reserved.
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
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
   Uri = uri:new( os:getenv("HERCULE_STORAGE", opts:val(storage, hercule)) ),
   RDF = rdf( jsonld(JsonLD) ),
   {ok, Sock} = esio:socket(uri:segments([scalar:s(Ns)], Uri)),
   case lens:get(lens:pair(<<"action">>, <<"put">>), Env) of
      <<"remove">> ->
         ok = elasticnt:remove(Sock, RDF);
      _ ->
         ok = elasticnt:put(Sock, RDF)
   end,
   esio:close(Sock),
   {ok, JsonLD}.

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
