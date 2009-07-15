-module(cdb_qlc).
-compile(export_all).
-include_lib("stdlib/include/qlc.hrl"). 

%% @doc This is the module to use with qlc.
%% Example: 
%% Q=(qlc:q([T#invoice.eid || T <- cdb_qlc:table(invoice), 
%% T#invoice.invno =:= 1050813899535])),
%% qlc:e(Q).
%%
%% There are multiple rooms of performance improvments here by making use of 
%% views and the querying options http://wiki.apache.org/couchdb/HTTP_view_API 
%% but for simplicity I use mnesia-calls here. 
%%

table(T) ->
    table(T,[]).
table(T,Opts) ->
    TF = fun() -> 		 
		 next(T) end,		 
    InfoFun = fun(keypos) -> 2;
		 (num_of_objects) -> mnesia:table_info(T,size);
                 (is_sorted_key) -> 	true;
                 (is_unique_objects) ->	 true;
                 (_U) -> undefined
              end,
    LookupFun =
        fun(2, Ks) ->
		lists:map(fun(K) ->
				  [R] =  mnesia:dirty_read(T,K),
				  {K,R}
			  end, Ks)
        end,
    DeafultOpts = [{info_fun, InfoFun},
		   {lookup_fun, LookupFun}],
    Opts1 = handle_options(Opts, DeafultOpts),
    qlc:table(TF, Opts1).
next([]) ->
    [];
next(T) ->
    next(T,mnesia:dirty_first(T)).

next(_T, '$end_of_table') ->
    [];
next(T, Key) ->
    receive
	{stop_next, _Caller} ->
	    [hd(mnesia:dirty_read(T, Key)) | fun() -> [] end]
    after 1 ->
	    
	    [hd(mnesia:dirty_read(T, Key)) |
	     fun() -> next(T, mnesia:dirty_next(T,Key)) end]
    end.
cdb_iter(T,infinity, ElementFun) ->
    cdb_iter(T, mnesia:dirty_first(T), -1, ElementFun);
cdb_iter(T,NElems, ElementFun) ->
    cdb_iter(T, mnesia:dirty_first(T), NElems-1, ElementFun).
cdb_iter(_T, '$end_of_table',_,_) ->
    [];
cdb_iter(_T,_K,0, _ElemFun) -> 
    []; 
cdb_iter(T,K,NElems, ElemFun) ->
    receive
	{stop_next,_Caller} ->
	    []
    after 1 -> ok
    end,	
    Rec = mnesia:dirty_read(T,K),
    Next = mnesia:dirty_next(T,K),
    [ElemFun(Rec) | cdb_iter(T,Next,NElems-1, ElemFun)].


handle_options([],DefaultOpts) -> DefaultOpts;
handle_options([ H={K,_} |T], DefaultOpts) ->
    DO = case proplists:is_defined(K, DefaultOpts) of
	     true ->
		 proplists:delete(K,DefaultOpts);
	     _ ->
		 DefaultOpts
	 end,
    [ H | handle_options(T, DO)].
   
    
