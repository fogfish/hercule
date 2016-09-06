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
%%   web-socket live stream resource
-module(hercule_live_q).

-export([
   allowed_methods/1,
   content_provided/1, 
   recv/3,
   send/3
]).

%%
allowed_methods(_Req) ->
   ['GET'].

%%
content_provided(_Req) ->
   [{application, json}].

%%
%%
recv(_Type, Datalog, {_Url, _Head, Env}) ->
   Pid  = self(),
   spawn(fun() ->
      stream:foreach(
         fun(X) ->
            Pid ! jsx:encode(X)
         end,
         hercule:q(lens:get(lens:pair(<<"ns">>), Env), Datalog)
      ),
      Pid ! eof
   end),
   ok.

%%
send(_Type, eof, {_Url, _Head, _Env}) ->
   eof;

send(_Type, Msg, {_Url, _Head, _Env}) ->
   {ok, Msg}.