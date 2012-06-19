%% Author: 77
%% Created: 2011-7-10
%% Description: TODO: Add description to sequence
-module(sequence).


-export([create_sequence/1,init_sequence/2,sequence/1,sequence/2]).
%%
%% API Functions
%%


%% --------------------------------------------------
%% sequence
%%


%% sequence table record
-record(sequence, {key, index}).

%% Creates sequence table. Performed once

create_sequence(Nodes) ->
  case catch mnesia:table_info(sequence,where_to_read) of
 	  {'EXIT', _} -> %% non-existing table
		   case mnesia:create_table(sequence, [{type, set},
				   {disc_copies, Nodes}, %%?
				   {attributes, record_info(fields, sequence)}
				   ]) of
			  {atomic,ok} ->
				   io:format("table ~p create success ~n",[sequence]),
				true;
    			{aborted,Reason}->
        			io:format("error:~p ~n",[Reason]),
        			false
		    end;
	  Node ->
 		    io:format("** Table sequence already exists on ~p, just entering data~n", [Node]),
        true
  end,
  ok = mnesia:wait_for_tables([sequence], 200000).
%%


%% Inits or resets a sequence to Value
init_sequence(Name, Value) ->
     {atomic, ok} =
	 mnesia:transaction(fun() ->
				   mnesia:write(#sequence{key=Name, index=Value})
			   end),
     ok.

%% Returns current value for sequence Name and increments
%% Sequence is created if not exists, and initial value 0 is returned.
sequence(Name) ->
     sequence(Name, 1).
%% increment sequence with Inc
sequence(Name, Inc) ->
     mnesia:dirty_update_counter(sequence, Name, Inc).
