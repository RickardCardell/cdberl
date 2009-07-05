-module(cdb_view).

-export([invoke_view/4, 
		parse_view/1,
		get_view_result/2,
		vals_from_view/4, 
		docs_from_view/4, 
		recs_from_view/4]).


%%%%
%%
%% http://wiki.apache.org/couchdb/HTTP_view_API
%%
%% Attribute options:    *
%% GET:
%%         {key, Keyvalue}
%%            {startkey, Keyvalue}
%%            {startkey_docid, Docid}
%%            {endkey, Keyvalue}
%%            {endkey_docid, Docid}
%%            {limit, Max_rows}
%%            {stale, ok}
%%            {descending, true}
%%            {skip, number of rows to skip
%%            {group, true}
%%            {group_level, int()}
%%            {reduce, false}
%%            {include_docs, true}
%%      POST:
%%            {"keys": ["key1", "key2", ...]} Trunk only (0.9)
%%
%%


invoke_view(Tab, Design, View, Attr) 
  when is_list(Design), is_list(View), is_list(Attr)->
    try(cdb_com:check_response_error(cdb_com:invoke_view(Tab, Design, View, Attr))) of    
       {ok,Response} -> Response
    catch
	_Err: _Why -> error_logger:error_msg("~p line ~p ~n~p ~p ~n ~p ~n",
			  [?MODULE,?LINE,_Err, _Why, erlang:get_stacktrace()]), 
		      throw({_Err,_Why})
    end.



vals_from_view(Tab, Design, View, Attr) ->
    get_values_from_view_result(invoke_view(Tab, Design, View, Attr)).

docs_from_view(Tab, Design, View, Attr) ->
    get_docs_from_view_result(invoke_view(Tab, Design, View, Attr)).


recs_from_view(Tab, Design, View, Attr) ->
    get_recs_from_view_result(Tab, invoke_view(Tab, Design, View, Attr)).





%%%%%%%%%%%%%% Internals %%%%%%%%%%%%%%%%%%	

parse_view({json,_}=JSON) -> 
    {_,_,Parsed} = erlang_couchdb:parse_view(JSON),
    Parsed.

get_view_result(_Tab, {struct,S}) ->
    Parsed = parse_view(S),
    L = [ Content  || {_Key, Content} <- Parsed],
    L.

get_values_from_view_result({struct,_}=Struct) ->
    Rows = cdb_com:get_value([<<"rows">>],Struct),	
    Bin_values = [ cdb_com:get_value([<<"value">>],R) || R <- Rows],
    Bin_values.

get_docs_from_view_result({struct,_}=Struct) ->
    Rows = cdb_com:get_value([<<"rows">>],Struct),	
    [cdb_com:get_value([<<"doc">>],R) || R <- Rows].
	
get_recs_from_view_result(Tab, {struct,_}=Struct) ->
    Rows = cdb_com:get_value([<<"rows">>],Struct),
    %% io:format("Rows: ~p ~n",[Rows]),
    [cdb_doc:doc_to_rec(Tab, cdb_com:get_value([<<"doc">>],R)) || R <- Rows].
	
