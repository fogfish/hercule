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
-include_lib("datum/include/datum.hrl").

-export([start/0]).
-export([
   deduct/2,
   entity/3,
   fact/4
]).

start() ->
   applib:boot(?MODULE, 
      code:where_is_file("sys.config")
   ).

%%
%%
deduct(Owner, Datalog) ->
   io:format(">>>>>>>>~n~s~n>>>>>>>>~n", [erlang:iolist_to_binary(Datalog)]),
   Script = datalog:p( scalar:c( erlang:iolist_to_binary(Datalog) ) ),
   case
      elasticlog:jsonify(
         elasticlog:q(
            datalog:c(elasticlog, Script, [{return, maps}]),
            implicitly(Owner),
            socket()
         )
      )
   of
      ?stream() ->
         {error, not_found};
      Stream    ->
         {ok, Stream}
   end.

%%
%%
entity(Owner, Bucket, Key) ->
   case
      pts:get(hercule, key(Bucket, Key), infinity)
   of
      {ok, Entity} ->
         allowed(Owner, Entity);
      {error, _} = Error ->
         Error
   end.

%%
%% {
%%    "@id": "subject"
%%    "predicate": "object"  
%% }
fact(Owner, Bucket, Key, JsonLD) ->
   case entity(Owner, Bucket, Key) of
      {error, not_found} ->
         pts:put(hercule, key(Bucket, Key), JsonLD#{<<"dc:creator">> => Owner}, infinity);
      {ok, _} ->
         pts:put(hercule, key(Bucket, Key), JsonLD#{<<"dc:creator">> => Owner}, infinity);
      Error ->
         Error
   end.

%%-----------------------------------------------------------------------------
%%
%% private
%%
%%-----------------------------------------------------------------------------

%%
%%
socket() ->
   [identity ||
      hercule_config:pool()
   ,  rand:uniform(_)
   ,  pns:whereis(socket, _)
   ].

%%
%%
key(Bucket, Key) ->
   [identity ||
      hercule_config:pool()
   ,  rand:uniform(_)
   ,  cats:unit({Bucket, _, Key})
   ].

%%
%%
implicitly(Owner) ->
   case hercule_config:owner_permit() of
      1 -> #{<<"dc:creator">> => Owner};
      _ -> undefined
   end.

%%
%%
allowed(Owner, Entity) ->
   case hercule_config:owner_permit() of
      1 ->
         case maps:get(<<"dc:creator">>, Entity, undefined) of
            Owner ->
               {ok, Entity};
            _ ->
               {error, forbidden}
         end;
      _ ->
         {ok, Entity}
   end.
