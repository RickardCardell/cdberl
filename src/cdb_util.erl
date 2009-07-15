-module(cdb_util).

-export([encode_key/2, 
	 decode_key/2, 
	 default_vals/1, 
	 rec_info/2, 
	 term_to_base64/1, 
	 base64_to_term/1, 
	 list_to_json_list/1, 
	 to_json/1, 
	 to_list/1, 
	 to_lower/1,
	 to_binary/1,
	 date_compare/3,
	 urlize/1,
	 urlize_attr/1]).
-include("cdb.hrl").

%% @doc This function can be used to have an arbitrary erlang term 
%%      as the primary key (_id) in CouchDb. See cdb_com:urlize for 
%%  further inspiration.
%% @spec encode_key(Key::term(), Rec::record()) -> string() | term()

%%encode_key(Key,Rec) when is_record(Rec, employee) orelse Rec == employee ->
%%    term_to_base64(Key);
encode_key(Key, _Rec) ->
    term_to_base64(Key).

%% @doc the opposite of encode_key/2. As the key == doc._id we need to convert 
%% it back to it's native form, for example if the key was an integer we 
%% will get a string back from CouchDB which we will need to convert back to 
%% an integer. Or if the key was a tuple we probably converted it to a json-form
%% as in cdb_doc:encode/1 or as a base64-string which we need convert back. That
%%  work is done here. See cdb_com:urlize for further inspiration.
%% @spec decode_key(Key::string(), Rec::(atom() | tuple())) -> term()

%%decode_key(Key, Rec) when is_record(Rec, employee) orelse Rec == employee ->
%%    base64_to_term(Key);
decode_key(Key, _Rec) ->
   base64_to_term(Key);
decode_key(Key, _Rec) ->
   base64_to_term(Key).


%% @spec default_vals(Tab::atom()) -> list()
%% @doc returns the attribute values of a record iff its a mnesia table or
%% matches any of the pattern down below. To add a table that is not in mnesia
%% just add a case below as:
default_vals(Tab) when is_atom(Tab) ->
    Len = length(mnesia_lib:val({Tab, attributes})),
    list_to_tuple([Tab | [ undefined || _ <- lists:seq(1,Len)]]).
				      

%% @spec rec_info( fields | size, Tab::atom()) -> list() | integer()
%% @doc Just as invoking record_info(Tab, fields | size). 
rec_info(fields,Tab) -> 
    mnesia_lib:val({Tab, attributes});
rec_info(size, Tab) ->
    1+ length(mnesia_lib:val({Tab, attributes})).

%% @spec term_to_base64(Term::term()) -> string()
%% @doc Encodes a term to a base64 representation. 
%% The opposite of base64_to_term/1
term_to_base64(Term) ->
    Funs =  [fun(X) -> term_to_binary(X) end,
	     %% here we can save up to 4 bytes if we are lucky with the input
	     %% fun(X) -> {_ , Data}= erlang:split_binary(X,1), Data end,
	     fun(X) -> encodeBase64(X) end, 	 
	     %% binary_to_list is idiotic but 
	     %% erlang_couchdb:update_document/4 doesn't handle binaries
	     fun(X) -> binary_to_list(X) end],
    lists:foldl(fun(F, V) -> F(V) end, Term, Funs).
%% @spec base64_to_term(String::string) -> term()
%% @doc The opposite of term_to_base64/1
base64_to_term(String) ->
    Funs = [fun(X) -> decodeBase64(X) end,
	    %% un(X) ->  list_to_binary([<<131>>, X]) end,
	    fun(X) -> binary_to_term(X) end],
    lists:foldl(fun(F, V) -> F(V) end, String, Funs).	 



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   
%%% Copied from CouchDB library (couch_util.erl).
%%% Purpose : Base 64 encoding and decoding.
%%% Copied from ssl_base_64 to avoid using the
%%% erlang ssl library

-define(st(X,A), ((X-A+256) div 256)).

%% A PEM encoding consists of characters A-Z, a-z, 0-9, +, / and
%% =. Each character encodes a 6 bits value from 0 to 63 (A = 0, / =
%% 63); = is a padding character.
%%

%%
%% encode64(Bytes|Binary) -> binary
%%
%% Take 3 bytes a time (3 x 8 = 24 bits), and make 4 characters out of
%% them (4 x 6 = 24 bits).
%%
encodeBase64(Bs) when list(Bs) ->
    encodeBase64(list_to_binary(Bs), <<>>);
encodeBase64(Bs) ->
    encodeBase64(Bs, <<>>).

encodeBase64(<<B:3/binary, Bs/binary>>, Acc) ->
    <<C1:6, C2:6, C3:6, C4:6>> = B,
    encodeBase64(Bs, 
		 <<Acc/binary, (enc(C1)), (enc(C2)), (enc(C3)), (enc(C4))>>);
encodeBase64(<<B:2/binary>>, Acc) ->
    <<C1:6, C2:6, C3:6, _:6>> = <<B/binary, 0>>,
    <<Acc/binary, (enc(C1)), (enc(C2)), (enc(C3)), $=>>;
     encodeBase64(<<B:1/binary>>, Acc) ->
	    <<C1:6, C2:6, _:12>> = <<B/binary, 0, 0>>,
	    <<Acc/binary, (enc(C1)), (enc(C2)), $=, $=>>;
	     encodeBase64(<<>>, Acc) ->
		    Acc.

%%
%% decodeBase64(BinaryChars) -> Binary
%%
decodeBase64(Cs) when is_list(Cs)->
    decodeBase64(list_to_binary(Cs));
decodeBase64(Cs) ->
    decode1(Cs, <<>>).

decode1(<<C1, C2, $=, $=>>, Acc) ->
	       <<B1, _:16>> = <<(dec(C1)):6, (dec(C2)):6, 0:12>>,
	       <<Acc/binary, B1>>;
decode1(<<C1, C2, C3, $=>>, Acc) ->
	       <<B1, B2, _:8>> = 
		   <<(dec(C1)):6, (dec(C2)):6, (dec(C3)):6, (dec(0)):6>>,
	       <<Acc/binary, B1, B2>>;
decode1(<<C1, C2, C3, C4, Cs/binary>>, Acc) ->
    Bin = <<Acc/binary, (dec(C1)):6, (dec(C2)):6, (dec(C3)):6, (dec(C4)):6>>,
    decode1(Cs, Bin);
decode1(<<>>, Acc) ->
    Acc.    

%% enc/1 and dec/1
%%
%% Mapping: 0-25 -> A-Z, 26-51 -> a-z, 52-61 -> 0-9, 62 -> +, 63 -> /
%%
enc(C) ->
    65 + C + 6*?st(C,26) - 75*?st(C,52) -15*?st(C,62) + 3*?st(C,63).

dec(C) ->
    62*?st(C,43) + ?st(C,47) + (C-59)*?st(C,48) - 69*?st(C,65) - 6*?st(C,97).


 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
%% @spec list_to_json_list(L::list()) -> list() 
%% @doc Returns the list as it was quoted
%% example 
%% to_json_list([1,2,3,a]) -> "[1,2,3,a]" 
list_to_json_list([]) -> "[]";
list_to_json_list([C]) -> "["++to_json(C) ++"]";		
list_to_json_list([H|T]) ->
    "[" ++ to_json(H) ++	
 	(lists:foldl(fun(C, Ack) ->
			     "," ++ to_json(C) ++ Ack end,
		     "",lists:reverse(T))) ++  "]".

to_json(V) when is_list(V) ->
    list_to_json_list(V);
to_json('_') ->
    "[]";%% needed in some view queries
to_json(V) ->
    to_list(V).


to_list(X) when is_integer(X) -> integer_to_list(X);
to_list(X) when is_float(X) -> float_to_list(X);
to_list(X) when is_atom(X) -> atom_to_list(X);
to_list(X) when is_list(X) -> X.

to_lower(S) ->
    to_lower(S,[]).

to_lower([], Ack) -> lists:reverse(Ack);
to_lower(["Å"|S], Ack) ->
    to_lower(S,["å"|Ack]);
to_lower(["Ä"|S], Ack) ->
    to_lower(S,["ä"|Ack]);
to_lower(["Ö"|S], Ack) ->
    to_lower(S,["ö"|Ack]);
to_lower([C|T], Ack) ->
    to_lower(T,[string:to_lower(C)|Ack]).

%% hrmpf..not so generalized code
to_binary(V) when is_atom(V) -> list_to_binary(atom_to_list(V));
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_integer(V) -> list_to_binary(integer_to_list(V));
to_binary(V) when is_float(V) -> list_to_binary(float_to_list(V));
to_binary(V) when is_binary(V) -> V.


date_compare(Pred, {Ms, S, Us1}, {Ms, S, Us2}) ->
    Pred(Us1, Us2);   
date_compare(Pred, {Ms, S1, _}, {Ms, S2, _}) ->
    Pred(S1, S2);
date_compare(Pred, {Ms1, _, _}, {Ms2, _, _}) ->
    Pred(Ms1, Ms2).

  

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
