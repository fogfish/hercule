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

-export([start/0]).
-export([q/2,q/3]).

start() ->
   applib:boot(?MODULE, 
      code:where_is_file("dev.config")
   ).

%%
%% evaluates datalog query and return stream
-spec q(_, _) -> datum:stream().
-spec q(_, _, _) -> datum:stream().

q(Ns, Datalog) ->
   q(Ns, #{}, Datalog).

q(Ns, Heap, Datalog)
 when is_function(Datalog) ->
   Uri = uri:new( opts:val(storage, hercule) ),
   {ok, Sock} = esio:socket(uri:segments([scalar:s(Ns)], Uri)),
   datalog:q(Datalog, Heap, Sock);

q(Ns, Heap, Datalog) ->
   q(Ns, Heap, elasticlog:c(scalar:c(Datalog))).
