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
%%   rest api: datalog management
-module(hercule_datalog).

-export([
   allowed_methods/1,
   content_provided/1, 
   content_accepted/1,
   'GET'/3,
   'PUT'/3
]).

%%
allowed_methods(_Req) ->
   ['PUT', 'GET'].

%%
content_provided(_Req) ->
   [{text, datalog}].

%%
content_accepted(_Req) ->
   [{text, datalog}].


'PUT'({text, datalog}, Datalog, {_Url, _Head, Env}) ->
   Id = lens:get(lens:pair(<<"id">>), Env),
   Fn = elasticlog:c( scalar:c(Datalog) ),
   ets:insert(hercule, {Id, Fn, Datalog}),
   ok.

'GET'({text, datalog}, _, {_Url, _Head, Env}) ->
   Id = lens:get(lens:pair(<<"id">>), Env),
   [{_, _, Datalog}] = ets:lookup(hercule, Id),   
   {ok, Datalog}.


% %%
% %%
% 'POST'(_Type, Datalog, {_Url, _Head, Env}) ->
%    Ns = lens:get(lens:pair(<<"ns">>), Env),
%    {200, jsx:encode( stream:list( hercule:q(Ns, Datalog) ) )}.
   