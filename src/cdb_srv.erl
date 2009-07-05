%%%-------------------------------------------------------------------
%%% File    : cdb_srv.erl
%%% Author  : Rickard Cardell <>
%%% Description : The gen server. Holds the revision cache, tab_to_server et al
%%  
%%%
%%% Created :  2009 by Rickard Cardell <>
%%%-------------------------------------------------------------------
-module (cdb_srv).
-export ([start_link/0,
	get_revision/2,
	set_revision/3,
	del_revision/2,
	del_revision/3,
	get_server/1,
	set_server/2]).

-behaviour (gen_server).
-export ([ init/1,
           handle_call/3,
           handle_cast/2,
           handle_info/2,
           code_change/3,
           terminate/2 ]).

-record (state, {tabs, rev_table}).
-record(server, {host="localhost", port=5984}).
%% record(tab, {id=default, server=#server{} }).
-define(REV_TABLE,(cdb_revision_cache)).


%% =====================================================================-
%%                                 Public                               -
%% =====================================================================-




set_server(Tab, Server) ->
  gen_server:call (?MODULE, { set_server, Tab, Server}).
  
get_server(Tab) ->
  gen_server:call (?MODULE, { get_server, Tab}).  

get_revision(Tab, Key) ->
  gen_server:call (?MODULE, { get_revision, Tab, Key }).

set_revision(Tab, Key, Rev) ->
  gen_server:call (?MODULE, { set_revision, Tab, Key, Rev }).

del_revision(Tab, Key) ->
  gen_server:call (?MODULE, { del_revision, Tab, Key, undefined}).
  
del_revision(Tab, Key, Rev) ->
  gen_server:call (?MODULE, { del_revision, Tab, Key, Rev}).

start_link () ->
  gen_server:start_link ({ local, ?MODULE }, ?MODULE, [], []).
 

%% =====================================================================-
%%                          gen_server callbacks                        -
%% =====================================================================-

init ([]) ->
  process_flag (trap_exit, true),
  Server = case application:get_env (cdberl, default_server) of
  		{ ok, {Host,Port}} -> #server{host = Host, port=Port};
  		_ -> #server{}
  	end,
  init_rev_table(),
  T = dict:new (),
  Tabs = dict:store(default, Server, T),
  { ok, #state{ tabs= Tabs , rev_table = ?REV_TABLE} }.


handle_call ({ set_server, Tab, Server = #server{host=_Host, port= _Port} }, 
  _From, State = #state{ tabs = Tabs }) ->
    { reply, ok, State#state{tabs = dict:store(Tab, Server, Tabs)}};

handle_call ({ get_server, Tab }, 
  _From={_Pid,_Ref}, 
  State = #state{ tabs = Tabs }) ->
	case dict:find (Tab, Tabs) of 
		{ ok, Server } -> 
		{ reply, Server, State };
		error ->     
			{ reply, get_default_server(Tabs), State }
	end;
 
%% @doc TODO: as for now every revision number for every table is stored in the
%% same place (by laziness). Though it is very easy to add another key for a 
%% tab, but I save a little space by not doing so. Also, the rev cache is never
%% cleared, unless the cdb_srv-process is stopped/exited of course. When there 
%% are need for it, It's easy to implment it.
handle_call ({ get_revision, _Tab, Key }, 
  _From, 
  State = #state{ rev_table = Rev_cache }) ->
	try ets:lookup(Rev_cache, Key) of 
			[{Key,Rev}] -> {reply, {ok, Rev}, State};
		[] ->{reply, {error, no_exists}, State}
	catch
		error : badarg -> {reply, {error, no_exists}, State}
	end;
  
handle_call (
 { set_revision, _Tab, Key, Rev }, _From, State=#state{rev_table=Rev_cache }) ->
   try(ets:insert(Rev_cache, {Key,Rev})) of
    		_ -> {reply, ok, State}
    	catch
    	 	_Err : _Why ->    {reply, ok, State}		
	end;

handle_call ({ del_revision, _Tab, Key, undefined }, 
  _From, State = #state{ rev_table = Rev_cache }) ->  
	try(ets:delete(Rev_cache, Key)) of
 		_ -> {reply, ok, State}  
 	catch
 		_Err : _Why -> {reply, ok, State}  
 	end;
handle_call (
  {del_revision,_Tab, Key, Rev }, _From, State =#state{ rev_table =Rev_cache})->
    try(ets:delete_object(Rev_cache, {Key, Rev})) of
       _ -> {reply, ok, State}  
    catch
	_Err : _Why -> {reply, ok, State}  
    end;

handle_call (_Msg, _From, State) ->
    { noreply, State }.

handle_cast (_Msg, State) ->
    { noreply, State }.

handle_info (_Msg, State) ->
    { noreply, State }.

code_change (_Vsn, State, _Extra) ->
    { ok, State }.

terminate (_Reason, _State= #state{ rev_table = Rev_cache }) ->
    ets:delete(Rev_cache),
    ok.


%% =====================================================================-
%%                          private		                        -
%% =====================================================================-


init_rev_table() ->
    %% first we check if the table exist	
    case ets:info(?REV_TABLE) of
   	undefined -> try(ets:new(?REV_TABLE, [named_table, public])) of
			?REV_TABLE ->{atomic, ok};
			_Other ->  {error, _Other}   	
		     catch
			 _Err:_Why -> {_Err,_Why} 
		     
		     end;
	_ -> {atomic, ok}
    end.

get_default_server(Dict) ->
    case dict:find(default, Dict) of
	{ok, Server} -> Server;
	_ -> #server{}
    end.

