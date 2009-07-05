%%% File    : cdb_index.erl
%%% Author  :  Rickard Cardell
%%% Description : A set of functions to imitate the mnesia:index* like 
%%%               index_read. Warning, ad_hoc code below! 
%%%               As an example this is a callback for cdb_tab:match_object(Pat)
%%%               where Pat is in reality an index_read so this function 
%%%               provides access to the CoudhDB view that is necessary. 
%%%               Because it can take a while to create a view in CouchDB its 
%%%               a good advice to create the view when the table (database) 
%%%               is small. 
%%%
%%% Created :  2009 by Rickard Cardell

-module(cdb_index).

-export([index_read/3, index_read/4]).



%% @spec index_read(Tab::atom(), Value:term(), Index_pos::integer()) -> 
%% record() | records()
%% @doc This function requires a view to be created before invocation
%% Example, for the table employee, we want to get employees that matches one 
%% certain value, town for example, which aren't the primary key. We need a
%% view to spit out the employees that lives in the town:
%%
%%  CouchDB view specifikation for the table employee, design employee:
%%
%%   design: 	employee
%%   view:	town
%% ---- javascript -------------------------------------------------
%%
%% function(doc) {
%%    if(doc.town){ 
%%       emit(doc.town, null);
%%    }
%% }
%% -----/javascript -------------------------------------------------
%%
%% OBSERVE: 1. at this point no views are created automatically. 
%%          2. there can only be one design in a table, otherwise 
%%             a very few functions won't work properly, for example
%%             cdb_com:all_keys/*
%%
%% To get all persons in table person that matches town = "Stockholm" : 
%% >index_read(person, "Stockholm, #person.town, true).
%% [#person{...}, #person{...},...]
%%
%% example of a function pattern
%% index_read(person, Town, #person.town) ->
%%    Attr = [{include_docs,true}, {key, cdb_util:to_json(Town)}], 
%%    cdb_view:recs_from_view(invoice, "person", "town", Attr);

index_read(Tab, Val, Index) when is_integer(Index) andalso Index > 1 ->
    Fields = cdb_util:rec_info(fields,Tab),
    Index_name = atom_to_list(lists:nth(Index-1, Fields)),
    index_read(Tab, Val, Index_name);
index_read(Tab, Val, Index) ->
    index_read(Tab, Val, Index, false).
%% @spec index_read(Tab::atom(), Val::term(), Index_name::string(), 
%% To_json::bool()) -> [record()] | records() 
%% @doc set To_json to false when the key is an integer or a data type that 
%% you want 'as it is'.
index_read(Tab, Val, Index_name, true) ->
    Json_val = cdb_util:to_json(Val),
    index_read(Tab, Json_val, Index_name, false);
index_read(Tab, Val, Index_name, false) ->   
    Attr = [{include_docs,true}, {key, Val}], 
    Design = atom_to_list(Tab),
    View = Index_name,
    cdb_view:recs_from_view(Tab, Design, View, Attr).
	
	

