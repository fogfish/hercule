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
   deduct/4,
   entity/3,
   fact/4
]).

start() ->
   applib:boot(?MODULE, 
      code:where_is_file("sys.config")
   ).

%%
%%
deduct(User, Bucket, N, Datalog) ->
   pts:call(hercule, Bucket, {deduct, User, N, Datalog}, infinity).

%%
%%
entity(User, Bucket, Key) ->
   case
      pts:call(hercule, Bucket, {entity, Key}, infinity)
   of
      {ok, #{<<"dc:creator">> := User} = Entity} ->
         {ok, Entity};
      {ok, _} ->
         {error, forbidden};
      {error, _} = Error ->
         Error
   end.

%%
%% {
%%    "@id": "subject"
%%    "predicate": "object"  
%% }
fact(User, Bucket, Key, JsonLD) ->
   case entity(User, Bucket, Key) of
      {error, not_found} ->
         pts:call(hercule, Bucket, {fact, JsonLD#{<<"dc:creator">> => User}}, infinity);
      {ok, _} ->
         pts:call(hercule, Bucket, {fact, JsonLD#{<<"dc:creator">> => User}}, infinity);
      Error ->
         Error
   end.


