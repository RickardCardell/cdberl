%% CouchDb document and record manipulation and conversion
%%---------------------------------------------------------------------------
%% @author Rickard Cardell <rickard.cardell@gmail.com>
%% @copyright 2008 Rickard Cardell 
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use, copy,
%% modify, merge, publish, distribute, sublicense, and/or sell copies
%% of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
%% BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
%% ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%---------------------------------------------------------------------------

-module(cdb_doc).

-include("cdb.hrl").
-export([rec_to_doc/1, 
	  doc_to_rec/2, 
	  to_record/3, 
	  from_record/2, 
	  set_value/2, 
	  set_value/3,
	  encode/1,
	 decode/1]).

-define(JSON_RESERVED_TUPLE,"tuple").
-define(JSON_RESERVER_BIGINT, "bigint").

%% @spec doc_to_rec(Tab::atom(), Doc:doc()) -> record::tuple().
%% @doc converts a document that is a member of Table to a record of type 
%% #Table{} 
doc_to_rec(Table , {json, Doc} ) -> doc_to_rec(Table,Doc);
doc_to_rec(Table, Doc) ->  doc_to_rec(default, Table , Doc).

doc_to_rec(default, Table , Doc) when is_atom(Table) ->
    Fields =  cdb_util:rec_info(fields, Table),
    Default_vals = cdb_util:default_vals(Table),
    to_record(Doc, Default_vals, Fields). 

 
%% @spec (Rec::record()) -> MochiwebJsonObject::jsonobj()
%% @doc Converts a record to a JsonObject by using the rfc4627-standard.
%%
%% Keys with the value undefined are truncated. 
rec_to_doc(Rec) ->
    Fields = cdb_util:rec_info(fields, element(1,Rec)),
    Doc = from_record(Rec, Fields),
    Doc.

set_value(Doc, {rev, Rev}) -> set_value(Doc, <<"_rev">>, Rev).
set_value(Doc, Key, Val) when not is_binary(Key)-> 
 	set_value(Doc, cdb_util:to_binary(Key), Val);
set_value(Doc, Key, Val) when not is_binary(Val)-> 
 	set_value(Doc, Key, cdb_util:to_binary(Val)); 
set_value(Doc, Key, Val) ->
 	Path = [Key],
 	New_doc = cdb_com:set_value(Path, Val, Doc),
 	New_doc.
 	
 
	
	
%% @spec to_record(JsonObject::jsonobj(), DefaultValue::record(), [atom()]) 
%% -> record()	
%% @doc  Converts a jsonobject to a record-tuple.
%% Copied and modified from the rfc4627 library by lshift, rfc4627.erl to 
%% be able to match binary_lists instead of lists.
%% Observe that if we can't match a value with a key in the record, 
%% the record's default-value is chosen, with no error in return
%%
%% - uses struct instead of rfc4627's obj-notation to function 
%%   with mochijson2:encode/decode in erlang_couchdb
to_record({struct, Values}, Fallback, Fields) ->
    list_to_tuple([element(1, Fallback) | 
		   decode_record_fields(Values, Fallback, 2, Fields)]).

decode_record_fields(_Values, _Fallback, _Index, []) ->
    [];
decode_record_fields(Values, Fallback, Index, [Field | Rest]) ->
    [case lists:keysearch(list_to_binary(atom_to_list(Field)), 1, Values) of
	 {value, {_, Value}} ->
	     decode(Value);
	 false ->
	     %% default value is chosen
	     element(Index, Fallback)
     end | decode_record_fields(Values, Fallback, Index + 1, Rest)].




%% @spec (Record, [any()]) -> jsonobj()
%% where Record = tuple()
%%
%% @doc Copied and somewhat modified from the rfc4627 library by lshift, 
%%		rfc4627.erl to avoid using that library
%%
%% - struct instead of obj
%% - keys as binary lists instead of lists
%%
%% Example:
%% -record(person,{fname, lname, age}). 
%% 
%% from_record(#person{fname="Rudolf",lname="mulen", age=45}, \
%% record_info(fields,person)).
%% {struct,[{<<"fname">>,"Rudolf"},
%%         {<<"lname">>,"mulen"},
%%         {<<"age">>,45}]}
%%  
from_record(R, Fields) ->
    {struct, encode_record_fields(R, 2, Fields)}.


encode_record_fields(_R, _Index, []) ->
    [];
encode_record_fields(R, Index, [Field | Rest]) ->
    case element(Index, R) of
	undefined ->
	    encode_record_fields(R, Index + 1, Rest);
	Value ->
	    [{list_to_binary(atom_to_list(Field)), encode(Value)} | 
	     encode_record_fields(R, Index + 1, Rest)]
    end.


%% @spec uses Lshifts rfc4627 with the addition that tuples are encoded to be a 
%% JSON-obj with the key ?JSON_RESERVED_TUPLE and the value is the result of 
%% as you would do tuple_to_list(Tuple). This is of course far from 
%% the optimal solution but we need a way to represent tuples in JSON  
%% Binaries cannot be encoded because an decoded binary is regarded as an atom
%% 
%% example: encode({123,456,678}). 
%% gives: {struct, [{?JSON_RESERVED_TUPLE,[123,456,678]}]}
%%         
%%

-define(json_tuple_out(E), {struct,[{?JSON_RESERVED_TUPLE, E}]}).
-define(json_tuple_in(E), {struct,[{<<?JSON_RESERVED_TUPLE>>, E}]}).

encode(T) ->
   (encode_element(T)).

encode_list([]) -> [];
encode_list(E) when not is_list(E) -> 
    %% a non proper list like [1,2,3 | atom ]
    error_logger:error_msg("Not a valid list: ~p, in module ~p line ~p~n",
			   [E, ?MODULE, ?LINE]),encode_list([E]); 

encode_list([H|T]) ->    	
	[ encode_element(H) | encode_list(T)].
    
encode_element(E) when is_tuple(E) ->
    ?json_tuple_out(encode_list(tuple_to_list(E)));
encode_element(E) when is_list(E) ->
    encode_list(E);
encode_element(E) ->
    E.

%% @spec this is the inverse of encode/1

decode(D) ->
   decode_tuples(D).

decode_tuples(?json_tuple_in(L)) when is_list(L) -> 
    list_to_tuple(lists:map(fun(X) -> decode_tuples(X) end, L));
decode_tuples(?json_tuple_out(L)) when is_list(L) -> 
    list_to_tuple(lists:map(fun(X) -> decode_tuples(X) end, L));
decode_tuples(L) when is_list(L) -> 
    lists:map(fun(E) -> decode_tuples(E) end, L);
decode_tuples(B) when is_binary(B) -> 
    %% XXX list_to_existing_atom kanske?
    list_to_atom(binary_to_list(B)); 
decode_tuples(V) -> V.
	    













-ifdef (DEBUG).    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Simple tests
%% 

%C = fun(X,Y) -> (cdb_doc:encode(X) > cdb_doc:encode(Y)) =:= (X > Y) end.

encode_test() ->
    T= fun(X) -> X1 = cdb_doc:encode(X), X2 = cdb_doc:decode(X1), X =:= X2 end, 
    
    Ints = [ 0,
	     97,
	     255,
	     2 bsl 61,
	     -456,
	     -2 bsl 61
	    ],
    Atoms = [abbc,
	     '_'],
   
	
    Tuples  = [{}, 
	       [ {X} || X <- Ints], 
	       [ {X} || X <- Atoms],
	       [ {X,Y} || X <- Atoms, Y <- Ints],
	       [ {X,Y} || X <- Atoms, Y <- Ints ]],
    
    Lists = [[],
	    [ [X] || X <- Tuples]		
	],
    
    Tuples2 = Tuples ++ [ [X] || X <- Tuples],
    Randoms = [ random_term () || _ <- lists:seq (1, 5000) ] ++ 
	[ random_variables () || _ <- lists:seq (1, 5000)],

    
    lists:map(fun(X) ->
		      case T(X) of
			  true -> ok;
			  _ -> io:format("Error: ~w ~n",[X]), exit(ds)
		      end end
	      
	      , Tuples2 ++ Lists ++ Atoms ++ Ints ++ Randoms ++  
	      [a,[ 1,2 | asd],a]).
    
	 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% random*-functions copied from the tcerl-library by Paul Mineiro 
%% http://code.google.com/p/tcerl/
%%
 random_integer () ->
   random:uniform (100000).
 
 random_float () ->
   random:uniform ().
 
 %%random_binary () ->
 %%  list_to_binary ([ random:uniform (255) || _ <- lists:seq (1, 5) ]).
 random_atom () ->
   list_to_atom ([ random:uniform ($z - $a) + $a || _ <- lists:seq (1, 5) ]).
 
 random_list () ->
   [ random_term () || _ <- lists:seq (1, 3) ].
 
 random_tuple () ->
   list_to_tuple (random_list ()).
 
 random_term () ->
   case random:uniform (8) of
     1 -> random_integer ();
     2 -> random_integer ();
     3 -> random_float ();
     4 -> random_float ();
     5 -> random_atom ();
     6 -> random_atom ();
     7 -> random_list ();
     8 -> random_tuple ()
   end.
 
 random_variable () ->
   case random:uniform (10) of
     1 -> 
       '_';
     2 ->
       list_to_atom ([ $$ ] ++ [ random:uniform ($z - $a) + $a || 
		     _ <- lists:seq (1, 5) ]); % not actually a variable
     _ ->
       list_to_atom ([ $$ ] ++ integer_to_list (random:uniform (100) - 1))
   end.
 
 random_variables () ->
   [ random_variable () || _ <- lists:seq (1, random:uniform (5)) ].
 
 random_matchhead () ->
   random_matchhead (random_variables ()).
 
 random_matchhead ([ H ]) ->
   case random:uniform (2) of
     1 -> { [ H ], H };
     2 -> { [], random_term () }
   end;
 random_matchhead ([ H | T ]) ->
   { V, M } = random_matchhead (T),
   case random:uniform (3) of
     1 -> { [ H | V ], { H, M } };
     2 -> { V, M };
     3 -> { V, { random_term (), M } }
   end.
 
 random_guard (_Variables) ->
   [].   % TODO
 
 random_matchbody (Variables) ->
   case { random:uniform (3), Variables } of
     { _, [] } -> [ '$_' ];
     { 1, _ } -> [ '$$' ];
     { 2, _ } -> [ '$_' ];
     { 3, _ } -> [ hd (Variables) | lists:filter (
		 fun (_) -> random:uniform (2) =:= 1 end, tl (Variables)) ]
   end.
 
 random_matchspec () ->
   [ (fun () -> { V, M } = random_matchhead (), 
                { M, random_guard (V), random_matchbody (V) } end) () ||
     _ <- lists:seq (1, random:uniform (3)) ].
 
 random_variable_or_term () ->
   case random:uniform (2) of
     1 -> random_term ();
     2 -> random_variable ()
   end.
 
 random_pattern (N) ->
   list_to_tuple ([ random_variable_or_term () || _ <- lists:seq (1, N) ]).

-endif.
