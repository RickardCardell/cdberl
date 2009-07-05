%%%-------------------------------------------------------------------
%%% File    : cdberlsup.erl
%%% Author  : Rickard Cardell <>
%%% Description : The supervising module of cdb. Much are copied straight from 
%%% tcerl and I haven't looked into the details of init/1
%%%
%%% Created : 2009 by Rickard Cardell <>
%%%-------------------------------------------------------------------
-module(cdberlsup).

-behaviour (supervisor).

-export ([ start_link/0, init/1 ]).

%% =====================================================================-
%%                                 Public                               -
%% =====================================================================-

start_link () ->
  supervisor:start_link ({ local, ?MODULE }, ?MODULE, []).

init ([]) ->
  { ok, 
    { { one_for_one, 3, 10 }, 
      [
        { cdb_srv,
          { cdb_srv, start_link, [] },
          permanent,
          5000,
          worker,
          [ cdb_srv ] 
        }
      ] 
    } 
  }.
