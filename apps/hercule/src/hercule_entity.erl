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
%%   rest api - entity key / val
-module(hercule_entity).

-export([
   allowed_methods/1,
   content_provided/1, 
   'GET'/3
]).

%%
allowed_methods(_Req) ->
   ['GET'].

%%
content_provided(_Req) ->
   [{application, json}].

%%
'GET'({application, json}, _, {_Url, _Head, Env}) ->
   [{_, Fn, _}] = ets:lookup(hercule, lens:get(lens:pair(<<"id">>), Env)),
   Urn  = lens:get(lens:pair(<<"urn">>), Env),
   Snap = hercule:q(lens:get(lens:pair(<<"ns">>), Env), #{urn => Urn}, Fn),
   {ok, jsx:encode( stream:list(Snap) )}.
