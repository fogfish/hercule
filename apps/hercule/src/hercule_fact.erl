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
   'POST'/3,
   'OPTIONS'/3
]).

%%
allowed_methods(_Req) ->
   ['OPTIONS', 'POST'].

%%
content_provided(_Req) ->
   [{application, json}].

%%
content_accepted(_Req) ->
   [{application, json}].

%%
'OPTIONS'({application, json}, _, {_Url, Head, _Env}) ->
   ok.

%%
'POST'({application, json}, Json, {_Url, Head, Env}) ->
   Ns  = lens:get(lens:pair(<<"ns">>), Env),
   Unique = lens:get(lens:pair(<<"unique">>, spo), Env),
   Uri = uri:new( os:getenv("HERCULE_STORAGE", opts:val(storage, hercule)) ),
   JsonLD = jsonld(Json),
   RDF = rdf(JsonLD),
   {ok, Sock} = esio:socket(uri:segments([scalar:s(Ns)], Uri)),
   case lens:get(lens:pair(<<"action">>, <<"put">>), Env) of
      <<"remove">> ->
         ok = elasticnt:remove(Sock, RDF, [{unique, scalar:atom(Unique)}]);
      _ ->
         ok = elasticnt:put(Sock, RDF, [{unique, scalar:atom(Unique)}])
   end,
   esio:close(Sock),
   {ok, jsx:encode( lists:usort([maps:get(<<"@id">>, X) || X <- JsonLD]) )}.

%%
%% decode binary to json-ld
jsonld(JsonLD) ->
   assert( decode(JsonLD) ).

decode(JsonLD) ->
   jsx:decode(JsonLD, [return_maps]).

assert(#{<<"@id">> := _} = Json) ->
   [Json];

assert(#{} = Json) ->
   Urn = <<"_:uid:", (bits:btoh( uid:encode( uid:g() ) ))/binary>>,
   [Json#{<<"@id">> => Urn}];

assert(List) ->
   lists:filter(
      fun(X) -> maps:is_key(<<"@id">>, X) end,
      List
   ).

%%
%% decode json-ld to type-safe knowledge facts
rdf(JsonLD) ->
   lists:flatten(
      lists:map(
         fun(X) -> semantic:typed(semantic:jsonld(X)) end,
         JsonLD
      )
   ).
