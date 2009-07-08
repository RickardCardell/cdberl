%%% File    : cdb_com.erl
%%% Author  : Rickard Cardell <>
%%% Description : This module is one layer above erlang_couchdb and couchdb. 
%%                Useful if we want to use ecouch or any other API instead 
%%                of erlang_couchdb. Every call to CouchDB should go via this 
%%                module.
%%% Created :  2009 by Rickard Cardell <>

-module(cdb_com).
-export([get_rev/2, 
	 get_doc/2, 
	 info/1, 
	 check_response_error/1, 
	 check_response_error/2, 
	 get_response_error/1, 
	 set_value/3,  
	 insert_docs/2, 
	 insert_doc/3, 
	 delete_doc/2, 
	 create_table/1, 
	 delete_table/1, 
	 init_table/2,
	 invoke_view/4,
	 last_key/1,
	 first_key/1,
	 next_key/2,
	 prev_key/2,
	 all_keys/1,
	 all_keys/2,
	 get_value/2,
     create_view/3,
	 urlize/1,
	 urlize_attr/1
        ]).

 
get_rev(Tab, Key) ->
	Doc_id = cdb_util:encode_key(Key,Tab),
	case erlang_couchdb:document_revision(get_server(Tab), Tab, Doc_id) of	
		{ok, _Doc_id, Bin_rev} when is_binary(Bin_rev) -> 
				Rev = binary_to_list(Bin_rev),
				{ok, Rev};
		{ok, _Doc_id, undefined} -> {error, {no_exists,  ""}};
		{error, {json, Struct}} -> get_response_error(Struct); 
		Error -> Error
	end.

get_doc(Table, Key)  when is_atom(Table) ->
    Doc_id = cdb_util:encode_key(Key,Table),
    {json, Response} =  erlang_couchdb:retrieve_document(
			  get_server(Table), 
			  atom_to_list(Table), Doc_id, []),
    case get_response_error(Response) of 
  	{error, Err} -> {error, Err};
  	_ -> Response
    end.   
   
info(Tab) ->  
    {ok, Info_list} =  erlang_couchdb:database_info(get_server(Tab), Tab),
    check_response_error({struct, Info_list}, Tab),
    Db_name = binary_to_list(proplists:get_value(<<"db_name">>, 
						 Info_list)),
    Doc_count =  proplists:get_value(<<"doc_count">>, Info_list),
    Doc_del_count = proplists:get_value(<<"doc_del_count">>, Info_list),
    Update_seq = proplists:get_value(<<"update_seq">>, Info_list),
    Compact_running =	proplists:get_value(<<"compact_running">>, Info_list),  
    Disk_size = proplists:get_value(<<"disk_size">>, Info_list),
    Instance_start_time= binary_to_list(
			   proplists:get_value(
			     <<"instance_start_time">>, Info_list)),   
    Doc_count =  proplists:get_value(<<"doc_count">>, Info_list),
    [{db_name, Db_name}, 
     {doc_count, Doc_count}, 
     {doc_del_count, Doc_del_count},
     {update_seq, Update_seq},
     {compact_running, Compact_running},
     {disk_size, (Disk_size div 4)},
     {instance_start_time, Instance_start_time},
     {doc_count, Doc_count}].



%% @private
%% get_response_error({json, Struct}::json()) -> 
%% {error, {Error, Reason}} | undefined 
get_response_error({struct,Dict}) ->
    F = fun(Z) -> 
		case is_binary(Z) of 
		    true -> binary_to_list(Z);
		    _ -> Z 
		end 
	end,
    Prop_list = lists:map(fun({X, Y}) -> 
				  {F(X), F(Y)} end, Dict), 
    case  {proplists:get_value("error", Prop_list), 
	   proplists:get_value("reason", Prop_list)} of
	%% No error, return as-is
	{undefined,_} ->
	    undefined;
	%% Not found, return an empty document
	{"not_found", Reason} ->
	    {error, {no_exists,  Reason}};
	{"conflict", Reason}  ->
	    {error, {revision_conflict,  Reason}}; 
	{"file_exists", Reason}  ->
	    {error, {already_exists, Reason}};    
	%% Other error
	{Err, Reason}   ->
	    {error, {Err,  Reason}}
    end.

check_response_error(Struct) -> check_response_error(Struct, undefined).
check_response_error({struct, Dict} = Struct, Victim) ->
    case lists:keysearch(<<"error">>, 1, Dict) of
	%% No error, return as-is
	false ->
	    {ok, Struct};
	_ -> case Victim of 
		 undefined -> 
       %%error_logger:error_msg(" ~p ~n ",[get_response_error(Struct)]), 
		     exit(get_response_error(Struct));
		 _ -> 
	%%error_logger:error_msg(" ~p~n ",[get_response_error(Struct)]), 
		     exit({get_response_error(Struct),Victim})
	     end
    end.


get_bulk_errors([], {_Oks, _Failes}=Ack) -> Ack;
get_bulk_errors([ Resp | T], {Oks, Failes}) ->
    case get_response_error(Resp) of
	undefined -> get_bulk_errors(T, {[{ok, Resp} | Oks], Failes});
	{error, _Err} -> get_bulk_errors(T, {Oks, [{Resp,_Err} |Failes]})
    end.


set_value(Path, Val, Doc) ->   		
    Ret = erlang_couchdb:set_value(Path, Val, Doc),
    Ret.

%% Docs is a list of tuples : {Key, Doc}
insert_docs(Tab, Docs) when is_list(Docs) -> 
    
    Flat_doc_list = lists:map(
		      fun({Key, Doc}) ->
			      Doc_id = cdb_util:encode_key(Key,Tab),
			      {struct, D} = 
				  set_value([<<"_id">>], 
					    list_to_binary(Doc_id), Doc),
			      D end, Docs),
    {json, Response} = 
	erlang_couchdb:create_documents(get_server(Tab), Tab, Flat_doc_list),
    {Oks, Failees} = get_bulk_errors(Response,{[],[]}),
    Fail_list = [  begin
			Id = binary_to_list(get_value([<<"id">>], S)),
			K = cdb_util:decode_key(Id ,Tab), K end 
			|| {S, _Err} <- Failees ],
	Ok_list = [ begin
			Id = binary_to_list(get_value([<<"id">>], S)),
			Rev = binary_to_list(get_value([<<"rev">>], S)),
			K = cdb_util:decode_key(Id ,Tab), 
			{K, Rev} 
		   end || {ok, S } <- Oks],
	{Ok_list, Fail_list}.
insert_doc(Tab, Key, Doc) -> 

    Doc_id = cdb_util:encode_key(Key,Tab),
 
    {json, Struct} = 
	erlang_couchdb:update_document(get_server(Tab), Tab, Doc_id, Doc),
 
    case get_response_error(Struct) of
	{error, Err} -> {error, Err};
	_ -> 
	     {ok, binary_to_list(erlang_couchdb:get_value([<<"rev">>] , 
							  Struct))}
    end.


delete_doc(Tab, Key) ->
    Doc_id = cdb_util:encode_key(Key,Tab),
    case cdb_rev:get_rev(Tab, Key) of		
	{ok, Revision} ->
	    %%we remove the entry of the key in rev_table
	    cdb_rev:remove_rev(Tab, Key),
	    {json,Response} = erlang_couchdb:delete_document(get_server(Tab), 
							     Tab, 
							     Doc_id, 
							     Revision),		
	    case get_response_error(Response) of
		undefined -> ok;
		{error, {revision_conflict, _Reason}} -> 	
		    %% we try again, this time we get revision-no 
		    %% from table, explicity
		    case cdb_rev:get_rev_from_database(Tab, Key) of
			{ok, New_revision} -> 
			    {ok, _Struct} = check_response_error(
					      erlang_couchdb:delete_document(
						get_server(Tab), 
						Tab, 
						Doc_id, 
						New_revision)),
			    ok;
			Err -> Err
		    end;
		{_Error, _Reason} -> {error, {_Error, _Reason}}			
	    end;
	{error, Err} -> {error, Err}
    end.




create_table(Tab) -> 
    Response = erlang_couchdb:create_database(get_server(Tab), Tab),
    case Response of
	ok-> {ok, Tab};
	{error, {json, Err}} -> 
	    
	    E = get_response_error(Err),
	    case E of
		{error, {already_exists,_}} ->  {ok, Tab};
		_ -> {error, E}
	    end
    end.




delete_table(Tab) ->
    Response = erlang_couchdb:delete_database(get_server(Tab), Tab),
    case Response of
	ok -> Tab;
	{error, {json, Struct}} -> {error,{no_exists, _}} = 
				       get_response_error(Struct),
				   {error,{no_exists,Tab}}
    end.




init_table(_Tab, InitFun) ->
    
    error_logger:error_msg("NOT IMPLEMENTED YET:"++
			   " cdb_com:init_table(Tab, InitFun) ~n",
			   []),
    catch(InitFun (close)),
    { error, fun_init_table_not_implemented_yet }.


invoke_view(Tab, Design, View, Attr) -> 
    invoke_view(get_server(Tab), Tab, Design, View, Attr).


%%invoke_view({Host::string(), Port::integer()}, Tab::atom(), Design::string()
%%, View::string(), Attr::string()) -> {struct, Struct::struct()}
invoke_view({_Host, _Port}=Server, Tab, Design, View, Attr) 
  when is_atom(Tab), is_list(Design), is_list(View), is_list(Attr)-> 
    
    Attr_urlized = urlize_attr(Attr),
    %io:format("~p ",[Attr_urlized]),
    
    {json, Response} = erlang_couchdb:invoke_view(Server, 
						  Tab, 
						  Design, 
						  View, 
						  Attr_urlized),
    Response.





last_key(Tab) ->	
    Limit =  [{limit,1},{descending, true}],
    case all_keys(Tab, Limit) of
	[Key] -> Key;
	[] -> '$end_of_table';
	_ -> exit(incorrect_key)
    end.

first_key(Tab) ->
    Limit = [{limit,2}],
    case all_keys(Tab, Limit) of
	[Key] -> Key;
	[] -> '$end_of_table';
	_ -> exit(incorrect_key)
    end.


next_key(Tab, Key) ->
    %%io:format("a next call..~p~n",[Key] ),
    Doc_id = cdb_util:encode_key(Key,Tab),
    Limit = [{limit,2}, {startkey, "\"" ++ Doc_id ++ "\""}, 
	     {descending,false}],
    %%io:format("in key: ~p two keys:..~p~n",[Key, all_keys(Tab, Limit)] ),
    case lists:reverse(all_keys(Tab, Limit)) of
	[Key , Next_key | _] -> Next_key;
	[Key] -> '$end_of_table';
	[L,R] -> error_logger:error_msg("Error in next_key: ~p~n ~p~n (~p)~n",
					[L,R,Key]), '$end_of_table'
    end.

prev_key(Tab, Key) ->
    Doc_id = cdb_util:encode_key(Key,Tab),
    Limit = [{limit,3}, 
	     {startkey, "\"" ++ Doc_id ++ "\"" }, 
	     {descending, true}],
    case lists:reverse(all_keys(Tab, Limit)) of
	[Key , Prev_key | _] -> Prev_key;
	[Key] -> '$end_of_table';
	_E -> exit(incorrect_key)
    end.

all_keys(Tab) ->
    all_keys(Tab, [{descending, true}]). 

%% @spec all_keys(Tab::atom(), Limit::tuplelist()) -> [] | value() | values() |
%% Exception.
%% @doc Retrieves all keys from table Tab and formatted/limited by limit. The 
%% keys are decoded by cdb_util:decode_key so in order to retreive the keys in 
%% their native data type format the cdb_util:decode_key/2 must be implemented 
%% for the table Tab otherwise the result will probably be be a list of strings.
%%
%% OBSERVE: There can only be one design-document in the table, for now, 
%% otherwise the current implementation of all_keys won't work (but that could 
%% be an easy fix).
%% 
%% See prev_key or next_key for examples of how to use Limit
%% Valid key-value tuples to use with in Limit (
%% from http://wiki.apache.org/couchdb/HTTP_view_API):
%%
%% {key,Keyvalue}
%% {startkey,Keyvalue}
%% {startkey_docid,docid}
%% {endkey=keyvalue}
%% {endkey_docid,Docid}
%% {limit,Max rows to return}
%% {stale,ok}
%% {descending,Bool}
%% {skip, Number of rows to skip}
%% {group, Bool}
%% {group_level,Int}
%% {reduce,Bool}
%% {include_docs,Bool}

all_keys(Tab, Limit) when is_list(Limit) ->
    {ok, Response} = 
	erlang_couchdb:retrieve_all_docs(get_server(Tab), Tab, Limit),
    check_response_error(Response),
    %% io:format("~p ~n ",[Response]),
    Key_dict= erlang_couchdb:get_value([<<"rows">>], Response),	
    try(	
      lists:foldl(
	fun(Struct, Ack) -> 
	     Id = binary_to_list(
		    erlang_couchdb:get_value([<<"id">>],Struct)),
        
        case lists:prefix("_design/", Id) of
		    true -> 
			%% the doc was a design_document, 
			%% we will ignore it
			Ack;
		    _ ->  			
			K = cdb_util:decode_key(Id ,Tab),
            %% here we get the revision number for free 
			%% so we store it locally
			Bin_rev =  
			    erlang_couchdb:get_value(
			      [<<"value">>,<<"rev">>],Struct),
			case Bin_rev of
			    undefined -> ok;
			    R -> cdb_rev:save_rev(Tab, K, binary_to_list(R))
			end,
			[K|Ack]
		end
	end, 
	[],
	Key_dict)	
     ) of Keys ->  %%io:format("~p ~n ",[Keys]),
	    Keys %lists:reverse(Keys)
    catch
	_W:_R -> error_logger:error_msg("Error: ~p ~p ~p~n",
					[_W,_R, erlang:get_stacktrace()])
    end.





create_view(Tab, Design_name, View) -> 
    {json, Struct} = erlang_couchdb:create_view(get_server(Tab), atom_to_list(Tab), Design_name, <<"javascript">>, View),
    case get_response_error(Struct) of
        undefined -> ok;
        {error, _}=Err -> Err
    end.
  



get_value(Path, {struct, _ } = Struct) when is_list(Path) ->
    erlang_couchdb:get_value(Path ,Struct).


get_server(Tab) ->
    {_,Host, Port} = cdb_srv:get_server(Tab), 
    %%io:format("~p:~p~n",[Host, Port]),
    {Host, Port}.


urlize(List) when is_list(List) ->
    urlize_list(List);    
urlize(Atom) when is_atom(Atom) ->
   "\"" ++ atom_to_list(Atom) ++ "\"";
urlize(Tuple) when is_tuple(Tuple) ->
    "{\"tuple\":" ++ urlize(tuple_to_list(Tuple)) ++ "}";
urlize(Bin) when is_binary(Bin) ->
    binary_to_list(Bin);
urlize(Int) when is_integer(Int) ->
    integer_to_list(Int);
urlize(Float) when is_float(Float) ->
    float_to_list(Float);
urlize(Pid) when is_pid(Pid) ->
    pid_to_list(Pid).

    
		    
		    


urlize_list([]) -> "[]";
urlize_list([C]) -> "["++urlize(C) ++"]";		
urlize_list([H|T]) ->
	"[" ++ urlize(H) ++	
	(lists:foldl(fun(C, Ack) ->
		"," ++ urlize(C) ++ Ack end,
		"",lists:reverse(T))) ++  "]".






urlize_attr(Attr) ->
    lists:map(fun({K,V} = Tuple) ->
			 L = [key,
			      startkey,
			      endkey,
			      startkey_docid,
			      endkey_docid],
			 case lists:member(K,L) of
			     true ->
				 {K,   list_to_binary(urlize(V))};
			     _ -> Tuple
			 end;
		  (Other) -> Other
		 end, Attr).
