%%%-------------------------------------------------------------------
%%% File    : cdb_tab.erl 
%%% Author  : Rickard Cardell <>
%%% Description : Entrance module for Mnesia's access to CouchDB. 
%%%               
%%% Created : 2009 by Rickard Cardell <>
%%%-------------------------------------------------------------------

-module(cdb_tab).
-behaviour(mnesia_ext).

-include_lib ("mnesia/src/mnesia.hrl").

-export([info/2, 
	 lookup/2, 
	 insert/2, 
	 match_object/2,  
         select/1, 
	 select/2, 
	 select/3, 
	 delete/2,
         match_delete/2, 
	 first/1, 
	 next/2, 
	 last/1,
         prev/2, 
	 slot/2, 
	 update_counter/3,
         create_table/2, 
	 delete_table/1, 
         add_index/2, 
	 delete_index/2,
         init_index/2, 
	 fixtable/2, 
	 init_table/3]).

-define (is_tabid (X), (is_atom (X) orelse
                        (is_tuple (X) andalso
                         is_atom (element (1, X))))).

-export([is_read_index/1, is_dollar/1, is_wildcard/1]).

%% @spec info(Tab::tabid(), What::any()) -> Value::any() | undefined
info(Tab, What) when ?is_tabid(Tab) ->
    try(cdb_ets:info(get_tab_id(Tab), What)) of
       Val -> Val
    catch
	_ : _Why -> undefined
    end.  

%%TODO: implement for bag perhaps
%% @spec lookup(Tab::tabid(), Key::term()) -> Value::list() | {error, Reason} 
lookup(Tab, Key) ->
     try(cdb_ets:lookup(get_tab_id(Tab), Key)) of
     	[] -> [];
     	L when is_list(L) -> L
     catch
	 _ : Why -> error_logger:error_msg(
		      "~p ~n ~p ~n",
		      [Why, erlang:get_stacktrace()]), {error, Why}
     end.



%% @spec insert(Tab::tabid(), Objects::objects()) -> ok | {error, Reason}
insert(_Tab, []) -> ok;
insert(Tab, Vals) when is_list(Vals) ->
    Cstruct = mnesia_lib:val ({ Tab, cstruct }),
    Access_type = Cstruct#cstruct.access_mode,
    case Access_type of
	read_write ->
	    case cdb_ets:insert(Tab, Vals) of
		ok  -> ok;
		Err -> Err
	    end;
	_read_only -> {error, {Tab, read_only}}
    end;
insert(Tab, Val) ->
    Cstruct = mnesia_lib:val ({ Tab, cstruct }),
    Access_type = Cstruct#cstruct.access_mode,
    case Access_type of
	read_write -> 
	    try cdb_ets:insert(get_tab_id(Tab), Val) of
		{ok, _Rev} -> ok;
		Err={error,_} -> Err
	    catch
		Err:Why -> {error,{Err,Why}} 
	    end;
	_read_only -> {error, {Tab, read_only}}
    end.


%% @spec delete(Tab::tabid(), Key::key()) -> ok | {error, Reason}
delete(Tab, Key) ->
    Cstruct = mnesia_lib:val ({ Tab, cstruct }),
    Access_type = Cstruct#cstruct.access_mode,
    case Access_type of 
	read_write -> 
	    try(cdb_com:delete_doc(get_tab_id(Tab),Key)) of
	       Val -> Val
	    catch
		_ : Why -> {error, Why}
	    end;
	_read_only ->  {error, {Tab, read_only}}
    end.
%% @doc see select/2 
match_object({Tab,{index, Ix}}, {K, '$1'}) ->
    cdb_index:index_read(Tab, K, Ix);
match_object(Tab, Pat) -> 
    case is_read_index(Pat) of
	    true -> %% this resembels an index_read 
		%% Can be implemented easily with cdb_index:index_read/3
		{Val, Ix} = get_index(Pat),
		cdb_index:index_read(Tab, Val, Ix);
	    _ ->	%% TODO 
		error_logger:error_msg(
		  "UNIMPLEMENTED FUNCTION CALLED: ~p ~n ~n",
		  [match_object]),
		exit(not_implemented_yet)
	end.

%% @spec select(Tab::tabid(), MatchSpec::match_specification()) -> 
%% [Match::object()] | {error, Reason}
%% @doc Here you can catch select calls and by parsing the match spec you can 
%% send it to the right view (will be very ad_hoc of course).
%% An mnesia:all_keys-call can be catched here without a view.
select(Tab, Pat) ->
 case is_all_keys(Pat) of
	true -> cdb_com:all_keys(get_tab_id(Tab));
        _ ->
	 error_logger:error_msg("UNIMPLEMENTED FUNCTION CALLED: ~p ~n ~p~n",
				[select,Pat]),
	 exit(not_implemented_yet)
 end.


%% TODO 
%% @spec select(Continuation::select_continuation()) -> {[Match::object()], 
%% More::select_continuation()} | '$end_of_table' | {error, Reason}
select(_Cont) ->
    error_logger:error_msg("UNIMPLEMENTED FUNCTION CALLED: ~p ~n ~n",[select]),
    exit(not_implemented_yet).


%% TODO 
select(_Tab, _Pat, _Limit) ->
error_logger:error_msg("UNIMPLEMENTED FUNCTION CALLED: ~p ~n ~n",[select]),
    exit(not_implemented_yet).




%% @spec  fixtable(Tab::tabid(), Bool::bool()) -> true
fixtable (Tab, _Bool) when ?is_tabid (Tab) ->
    true.



%% TODO 
match_delete(_Tab, _Pat) ->
    error_logger:error_msg("UNIMPLEMENTED FUNCTION CALLED: ~p ~n~n",
			   [match_delete]), 
    exit(not_implemented_yet).



%%@spec first(Tab::tabid()) -> Key::any() | '$end_of_table'
first(Tab) ->
    try(cdb_com:first_key(get_tab_id(Tab))) of
	    K -> K
	 catch
	       _Why:_Reason -> 
		 error_logger:error_msg("~p ~p~n",[_Why, _Reason]), 
		 exit({_Why,_Reason})
	   end.


%% @spec next(Tab::tabid(), Key::any()) -> NextKey::any() | '$end_of_table'
next(Tab, Key) ->
    try(cdb_com:next_key(get_tab_id(Tab), Key)) of
    K -> K
   catch
       _Why:_Reason -> 
	   error_logger:error_msg("~p ~p~n ~p ~n",
				  [_Why, _Reason, 
				   erlang:get_stacktrace()]), '$end_of_table'
   end.



%% @spec last(Tab::tabid()) -> Key::any() | '$end_of_table'
last(Tab) ->
	try(cdb_com:last_key(get_tab_id(Tab))) of
	   	K -> K
	   catch
	       _Why:_Reason -> 
		   error_logger:error_msg("~p ~p~n ~p",
					  [_Why, _Reason,  
					   erlang:get_stacktrace()]), 
		   exit({_Why,_Reason})
	   end.


%%@spec prev(Tab::tabid(), Key::any()) -> NextKey::any() | '$end_of_table'
prev(Tab, Key) ->	
   try(cdb_com:prev_key(get_tab_id(Tab), Key)) of
   	K -> K
   catch
   	_Why:_Reason -> error_logger:error_msg("~p ~p~n",[_Why, _Reason]), 
					exit({_Why,_Reason})
   end.

slot(_Tab, _Pos) ->
error_logger:error_msg("UNIMPLEMENTED FUNCTION CALLED: ~p ~n ~n",[slot]),
    exit(not_implemented_yet).

update_counter(_Tab, _C, _Val) ->
error_logger:error_msg("UNIMPLEMENTED FUNCTION CALLED: ~p ~n ~n",
	[update_counter]),
    exit(not_implemented_yet).

%% create_table(Tab::tabid(), Cs::cstruct()) -> Tab | {error, Reason}
create_table(Tab, Cs) ->
    Tabid = get_tab_id(Tab),
    { _, ordered_set, _ } = Cs#cstruct.type,
    Response = cdb_com:create_table(Tabid),
    case Response of
	{ok, _} -> Tabid;
	_Other-> {error, _Other}
    end. 

%% @spec delete_table(Tab::tabid()) -> {ok, Tab} | {error, Reason}
delete_table(Tab) -> 
    Tabid = get_tab_id(Tab),
    Response =cdb_com:delete_table(Tabid),
    case Response of
	{ok, _} -> {ok, Tab};
	Err -> Err
    end.

%% TODO 
%% add a view that corresponds to an ordinary index
add_index(_Tab, _Pos) ->
	ok.
    
%% TODO 
delete_index(_Tab, _Pos) ->
   ok.
    
%% TODO 
init_index(_Tab, _Pos) ->
    case cdb_com:info(_Tab) of
    {error, _Cause } ->%% table exists in schema but not on server, what to do?
    			{error, no_exists};

    _ -> ok
    end.

    

init_table(_Tab, _InitFun, _Sender) ->
     cdb_com:init_table(_Tab, _InitFun).


%%=====================================================================-
%%                               Private                               -
%%=====================================================================-

%% @hidden
get_tab_id(Tab) when is_atom(Tab) ->
    Tab;
get_tab_id({Tab, {{T1,T2,T3},_Node}}) -> %% checkpoint retainer
    list_to_atom(atom_to_list(Tab) ++ integer_to_list(T1) ++ 
		 integer_to_list(T2) ++integer_to_list(T3) ++ "_ret").
		 
%% @hidden
%%@spec get_index(Pat::tuple()) -> {Var:term(), Index_pos:int()} | Exception
get_index(Pat) ->
	get_ix(tl(tuple_to_list(Pat)),2).
	
get_ix([ H | T], Pos) ->
	case is_wildcard(H) of 
		     true ->  get_ix(T, Pos+1);
			_ ->  {H, Pos}
	end.	
	
%% @hidden
%% @spec is_read_index(T::tuple()) -> true | false
%% @doc returns true iff there are exactly 2 non wildcard and 
%% non dollar terms in T and 0 dollar terms in T.
%% Example for record person, {person, name, age}
%% the following pattern is a pattern for matching out the age:
%% {person, '_', 31} i.e #person{age=31, _='_'} which is actually 
%% a read on a index where the index is on the age:
%% is_read_index(#person{age=31, _='_'}) -> true
%% is_read_index(#person{name="Rickard", _='_'}) -> true
%% is_read_index(#person{age=31, name='$1'}) -> false
%% is_read_index(#person{}) -> false
is_read_index(T) when is_tuple(T) ->
	{2, true} =:= lists:foldl( fun(X,{C, Valid}) -> 
		case {is_dollar(X), is_wildcard(X)} of
		{true, false} -> {C, false};
		{false, true} -> {C, Valid};
		 _ -> {C+1, Valid}
		 end
		end,
		{0, true}, tuple_to_list(T)).

	
	



is_dollar(V) when is_atom(V) ->
	lists:prefix("$", atom_to_list(V));
is_dollar( _V ) -> false.

is_wildcard('_') -> 	true;
is_wildcard('$_') -> 	true;
is_wildcard( _ ) -> 	false.


%% @doc pattern to match :{{tabname, '$1', _= '_'}, [], ['$1']}},
%% resembles the pattern when calling ets:all_keys(Tab) 
is_all_keys([{Pat,Guard, Result}]) ->
   T_form = {1,1} =:= lists:foldl( fun(X,{C1, C2}) -> 
		case {is_dollar(X), is_wildcard(X)} of
		{true,false} -> {C1, C2 +1};
		{false, true} -> {C1,C2};
		 _ -> {C1+1, C2}
		 end
		end,
		{0, 0}, tuple_to_list(Pat)),
   T_guard = [] =:= Guard,
   T_result = [element(2,Pat)] =:= Result,
   T_form andalso T_guard andalso T_result.	   






