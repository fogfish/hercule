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
-module(hercule_sup).
-behaviour(supervisor).

-export([
   start_link/0, init/1
]).

-define(CHILD(Type, I),            {I,  {I, start_link,   []}, permanent, 5000, Type, dynamic}).
-define(CHILD(Type, I, Args),      {I,  {I, start_link, Args}, permanent, 5000, Type, dynamic}).
-define(CHILD(Type, ID, I, Args),  {ID, {I, start_link, Args}, permanent, 5000, Type, dynamic}).

%%-----------------------------------------------------------------------------
%%
%% supervisor
%%
%%-----------------------------------------------------------------------------

start_link() ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, []).
   
init([]) ->   
   {ok,
      {
         {one_for_one, 4, 1800},
         [
            global_pool()
         ,  bucket_pool()
         ,  restapi()
         ]
      }
   }.

%%
%%
global_pool() ->
   ?CHILD(supervisor, hercule_socket_sup, 
      [
         hercule_config:pool()
      ,  hercule_config:storage()
      ]
   ).

%%
%%
bucket_pool() ->
   ?CHILD(supervisor, pts,
      [
         hercule,
         [
            'read-through',
            {keylen,    2},
            {entity,    hercule_bucket},
            {factory,   permanent}
         ]
      ]
   ).

%%
%%
restapi() ->
   restd:spec(
      hercule_restapi:endpoints(),
      hercule_restapi:filters(),
      #{
         port => opts:val(port, hercule)
      ,  backlog => 1024
      ,  sock =>  so(uri:new(opts:val(port, hercule)))
      }
   ).

so({uri, http, _}) ->
   #{};

so({uri, https, _}) ->
   #{
      certfile => certificate()
   ,  keyfile => private_key()
   }.


certificate() ->
   File = filename:join([code:priv_dir(hercule), "certificate.pem"]),
   {ok, Steam} = s3am:fetch(opts:val(tls_certificate, hercule)),
   ok = file:write_file(File, stream:list(Steam)),
   File.


private_key() ->
   File = filename:join([code:priv_dir(hercule), "private_key.pem"]),
   {ok, Steam} = s3am:fetch(opts:val(tls_private_key, hercule)),
   ok = file:write_file(File, stream:list(Steam)),
   File.
