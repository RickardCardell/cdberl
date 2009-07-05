%%%-------------------------------------------------------------------
%%% File    : cdb_rev.erl
%%% Author  : Rickard Cardell <>
%%% Description : 
%%%
%%% Created :  2009 by Rickard Cardell <>
%%%-------------------------------------------------------------------
-module(cdb_rev).

-export([get_rev_from_store/2, 
	get_rev_from_database/2, 
	get_rev/2, 
	remove_rev/2, 
	remove_rev/3, 
	save_rev/3]).
  	
get_rev_from_store(Tab, Key) -> 
 	cdb_srv:get_revision(Tab,Key).
	
get_rev_from_database(Tab, Key) -> 
	case cdb_com:get_rev(Tab, Key) of
		{ok, Rev} ->%% and we store it
			save_rev(Tab, Key, Rev),
			{ok, Rev};
	R={error, _Error} -> R
	end.
		

get_rev(Tab, Key) -> 	 	
    case get_rev_from_store(Tab, Key) of 		 			
	R = {ok, _Rev} -> R;
	_ ->%% o revision-# stored locally, we look in table 
	    get_rev_from_database(Tab, Key)
    
    end.


remove_rev(Tab, Key) ->
    cdb_srv:del_revision(Tab, Key).
remove_rev(Tab, Key, Rev) ->
    cdb_srv:del_revision(Tab, Key, Rev).



save_rev(Tab, Key, Rev) -> 
    cdb_srv:set_revision(Tab, Key, Rev).
