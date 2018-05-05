FROM fogfish/erlang-alpine-rt:20.2

COPY _build/default/rel /rel

ENTRYPOINT spawn-erlang-node hercule
