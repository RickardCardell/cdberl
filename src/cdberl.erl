%% @doc The cdberl application.  It must be started before any of the 
%% functions provided by this library can be used.
%% @end

-module (cdberl).
-export ([ start/0,
           stop/0 ]).
-behaviour (application).
-export ([ start/2,
           stop/1 ]).

%% =====================================================================-
%%                                 Public                               -
%% =====================================================================-

%% @spec start () -> ok | { error, Reason }
%% @equiv application:start (cdberl)
%% @end

start () ->
  Res = application:start (cdberl),
  
  case Res of
    ok ->   ok;
    {error,{already_started,cdberl}} -> ok;
    Err -> Err
  end.

%% @spec stop () -> ok | { error, Reason }
%% @equiv application:stop (cdberl)
%% @end

stop () ->
  application:stop (cdberl).

%% =====================================================================-
%%                         application callbacks                        -
%% =====================================================================-

%% @hidden

start (_Type, _Args) ->
  cdberlsup:start_link ().

%% @hidden

stop (_Args) ->
  ok.
