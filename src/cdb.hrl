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
%%-record(employee, {name=undefined, age=undefined, salary = undefined, 
%%					ssn=undefined , postnr =undefined,
%% town=undefined }). 
%%-record(iplaywow,{playerid, level, name, type}).
%%-record(family, {year, name, fruit, int}).



-record(employee, {name=undefined, age=undefined, salary = undefined, 
                  ssn=undefined , postnr =undefined,
 town=undefined }). 

-define(clicks(V), cdb_dbg:clicks(V)).
-define(clicke(V), cdb_dbg:clicke(V)).
-define(clickeval(V), cdb_dbg:clickeval(V)).

