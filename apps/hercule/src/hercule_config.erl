-module(hercule_config).

-export([
   pool/0
,  elastic/0
,  owner_permit/0
]).

pool() ->
   typecast:i(opts:val(pool, 32, hercule)).

elastic() ->
   uri:new(opts:val(elastic, "http://localhost:9200/*", hercule)).

owner_permit() ->
   typecast:i(opts:val(owner_permit, 1, hercule)).