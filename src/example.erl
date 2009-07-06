-module(example).
-compile(export_all).

-include("example.hrl").


%%9> rr("src/example.hrl").
%%[employee]
%%10> example:add_emp(#employee{name="RCardell",age=31, salary=0, ssn=1234, postnr=13133}).
%%{atomic,ok}
%%11> example:get_emp("RCardell").
%%{atomic,[#employee{name = "RCardell",age = 31,salary = 0,
%%                   ssn = 1234,postnr = 13133,town = undefined}]}


create_table(Tab) ->
     mnesia:create_table (Tab, [ 
            { type, { external, ordered_set, cdb_tab } }, 
            { external_copies, [ node () ] }, 
            { attributes, record_info(fields, employee)}
            ]).
            
            
            
add_emp(Emp) when is_record(Emp, employee) ->
    Fun = fun() ->
            mnesia:write(Emp)
        end,
    mnesia:transaction(Fun).

get_emp(Name) ->
    Fun = fun() ->
                mnesia:read(employee, Name)
            end,
    mnesia:transaction(Fun).
    

