-module(cdb_ets).

-export([info/2, insert/2, lookup/2]).



%% @spec info(Tab::atom(), What::any()) -> Value::any() | undefined | Exception
info(Tab, name) -> {Tab, name};
info(Tab,no_objects) -> info (Tab, doc_count);
info(Tab,memory) -> info (Tab, disk_size);
info(Tab,size) -> info (Tab, doc_count);
info (Tab, Item) -> 
    Info_list = info (Tab), 
    case Item of
	X when is_list(Info_list) andalso 
	       (X =:= size orelse X =:= file_size orelse X =:= disk_size) ->
	    proplists:get_value (X, Info_list);
	doc_count when is_list (Info_list)-> 
	    proplists:get_value (doc_count, Info_list);
	_Other -> undefined
    end.
		
%% TODO: perhaps fetching rev-number before inserting
%% bulk can improve average performance
insert(Tab, Recs)  when is_list(Recs) ->   
    Docs = [ do_convert(R)|| R <- Recs],    %% a list of {Key, Struct}
    {Oks, Failes} = cdb_com:insert_docs(Tab, Docs),	    
    %% foreach "failed" document we insert them one at a time
    Failed_twice_docs = 
        lists:foldl(fun(K, Ack) ->
			    io:format("looking up K ~n"),
			    {value,{K, Doc}} = lists:keysearch(K, 1, Docs),
      			    case insert(Tab, K, Doc) of
				{ok,_} ->  Ack;
				{error, _}=Err -> [Err|Ack]
			    end
		    end,
		    [],Failes),
    %%insert OK:s rev in store
    [ cdb_rev:save_rev(Tab, K, R) || {K, R} <- Oks ],	
    %% check if every doc was successfully inserted
    case  Failed_twice_docs =:= [] of
	true -> ok;
	_ -> {error, Failed_twice_docs}
    end;
insert(Tab, Rec) ->
    {Key, Doc} = do_convert(Rec),
    insert(Tab, Key, Doc).

insert(Tab, Key, {struct,_} = Doc) ->
    %% at first we check in the ets-table. If it exists there, the document 
    %% 	exists in the database. Some overhead but acceptable
    Response = case cdb_rev:get_rev_from_store(Tab, Key) of
		   {ok, Rev} -> %% looks like we are updating
		       do_insert(Tab, Doc, Key, Rev);
		   _Err -> 
		       do_insert(Tab, Doc, Key, undefined)
	       end,	
    %% if the server response are {error, revision_conflict} 
    %% then we retrieve the revision number from database 
    %% and try to update the database document    
    case Response of
	{ ok, _New_rev} -> 
	    { ok, _New_rev}; 			
	{error, {revision_conflict,_}} -> 
	    %% we now need to fetch the latest rev-no. 
	    %% we then resend the insert-query
	    case cdb_rev:get_rev_from_database(Tab, Key) of 
		{ok, New_rev} -> 
		    do_insert(Tab, Doc, Key, New_rev);
		%% it can be that the value existed in ?REV_TABLE but 
		%% that the document was deleted from database.
		%% In this case CouchDB will set the new revision number 
		%% based on the revision number we passed instead of 
		%% generating a new one (which doesn't matter so no action have 
		%% to be taken)
		_Other -> {error, _Other}
	    end;		
	{_Ret, undefined} -> {error, could_not_insert_document};
	_Other -> {error, _Other}			
    end.




lookup(_Tab, []) -> [];
lookup(Tab, Key) -> 
    case cdb_com:get_doc(Tab, Key) of
	{error, _Err} -> [];
	Doc -> [cdb_doc:doc_to_rec(Tab, Doc)]
    end.

get_key(Record) when is_tuple(Record) andalso tuple_size(Record) > 1 ->
    element(2, Record).




%% =====================================================================-
%%                                Private                               -
%% =====================================================================-

info(Tab) ->
    cdb_com:info(Tab).

do_insert(Tab, Doc, Key, undefined) ->
    Resp = cdb_com:insert_doc(Tab, Key, Doc),
    case Resp of 		
	R = { ok, _New_rev} ->  
	    cdb_rev:save_rev(Tab, Key, _New_rev), 
	    R; 			
	R = {error, {revision_conflict,_}} ->
	    %% remove rev cause it was outdated
	    cdb_rev:remove_rev(Tab, Key),
	    R;
	R = {error, _ } -> R;
	{_Ret, undefined} -> {error, could_not_insert_document}
    end;
do_insert(Tab, Doc, Key, Rev) -> 	
    Doc1 = cdb_doc:set_value(Doc, {rev, Rev}),
    do_insert(Tab, Doc1, Key, undefined).	



do_convert(Rec)  ->
    Key = get_key(Rec),
    Struct = cdb_doc:rec_to_doc(Rec), 
    {Key, Struct}.
