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
-module(hercule).
-compile({parse_transform, category}).

-export([start/0]).
-export([
   deduct/2,
   entity/2,
   fact/2
]).

start() ->
   applib:boot(?MODULE, 
      code:where_is_file("sys.config")
   ).

%%
%%
deduct(Bucket, Datalog) ->
   pts:call(hercule, Bucket, {deduct, Datalog}, infinity).

%%
%%
entity(Bucket, Key) ->
   pts:call(hercule, Bucket, {entity, Key}, infinity).

%%
%% {
%%    "@id": "subject"
%%    "predicate": {"@value": "object", "@type": "type"}  
%% }
fact(Bucket, #{<<"@id">> := _} = JsonLD) ->
   [either ||
      cats:unit( jsonld_to_facts(JsonLD) ),
      cats:unit( [patch(Bucket, X) || X <- _] ),
      cats:sequence(_)
   ];

fact(Bucket, #{<<"s">> := S, <<"p">> := P, <<"o">> := O, <<"type">> := Type}) ->
   patch(Bucket, encode(S, P, O, Type)).

jsonld_to_facts(#{<<"@id">> := S} = JsonLD) ->
   [encode(S, P, O, Type) ||
      {P, #{<<"@value">> := O, <<"@type">> := Type}} <- maps:to_list(JsonLD),
      P =/= <<"@id">>
   ].

patch(Bucket, Fact) -> 
   pts:call(hercule, Bucket, {fact, Fact}, infinity).

%%
%%
encode(S, P, O, Type) ->
   #{
      s    => semantic:compact(S), 
      p    => semantic:compact(P), 
      o    => O, 
      type => semantic:compact(Type)
   }.


