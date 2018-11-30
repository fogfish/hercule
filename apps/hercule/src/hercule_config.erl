-module(hercule_config).

-export([
   pool/0
,  storage/0
,  owner_permit/0
]).

pool() ->
   typecast:i(opts:val(pool, 32, hercule)).

storage() ->
   uri:new(opts:val(storage, "http://localhost:9200/*", hercule)).

owner_permit() ->
   typecast:i(opts:val(owner_permit, 1, hercule)).