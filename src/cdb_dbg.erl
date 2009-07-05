%%% File    : cdb_dbg.erl
%%% Author  : RC
%%% Description : A small module to measure execution times of code between 
%%% modules or calls (no guarantees that the measuring is correct in any way). 
%%% Good for analyzing the execution time of the http-call in erlang_couchdb 
%%% and the conversion time of record_to_JSON for example.   
%%% Example:
%%%
%%% foo() ->
%%%    ...
%%%    clicks(1),
%%%    do_heavy_job(),
%%%    clicke(1),
%%%    ...
%%%    %%output result:
%%%    clickeval(1),

-module(cdb_dbg).
-export([clicks/1, clicke/1, clickeval/1, check/1]). 

-ifdef(debug). %% debug must be defined in order to work
clicks(Checkpoint) -> put({Checkpoint},{cdb_test:current_timestamp()}).

clicke(Checkpoint) ->
    K = {Checkpoint},
    {V1} = get(K),
    put(K, {V1, cdb_test:current_timestamp()}).


clickeval(Checkpoint) -> 
    io:format("Result ~p: ~p ~n",[ Checkpoint, check(erase({Checkpoint}))]).

check(undefined)-> na;
check({A,B}) when is_integer(A), is_integer(B) -> {A,B, B-A};
check({V}) -> {single, V};
check(_) -> na.
-else.
clicks(_Checkpoint) -> ok.

clicke(_Checkpoint) -> ok.


clickeval(_Checkpoint) -> ok.

check(_)-> ok.
-endif.
