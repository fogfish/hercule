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
-module(hercule_snap_q).

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
   [{text, datalog}].


'POST'({application, json}, Datalog, {Url, _Head, Env}) ->
   Snap = hercule:q(lens:get(lens:pair(<<"ns">>), Env), Datalog),
   case scalar:i( uri:q(<<"n">>, 0, Url) ) of
      0 ->
         {ok, jsx:encode( stream:list(Snap) )};
      N ->
         {ok, jsx:encode( stream:list(N, Snap) )}
   end.
