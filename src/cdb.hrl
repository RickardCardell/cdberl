%%%-------------------------------------------------------------------
%%% File    : cdb.hrl
%%% Author  : Rickard Cardell <>
%%% Description : Here you can specify the records/tables you want to use with
%%  cdberl. 
%%%
%%% Created : 2009 by Rickard Cardell <>
%%%-------------------------------------------------------------------

%%-define(debug,false).
%%-record(testtab, {
%%         foo,
%%          bar,
%%          baz
%%         }).


-define(clicks(V), cdb_dbg:clicks(V)).
-define(clicke(V), cdb_dbg:clicke(V)).
-define(clickeval(V), cdb_dbg:clickeval(V)).

