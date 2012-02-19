%%% -------------------------------------------------------------------
%%% Author  : 77
%%% Description :
%%%
%%% Created : 2011-7-2
%%% -------------------------------------------------------------------
-module(node_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("meta_schema.hrl").
-include("cmd_shema.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, sub_process/5, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(node_state, {}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

stop() ->
	  gen_server:cast(?MODULE, stop).


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	process_flag(trap_exit,true),  
    {ok,#node_state{}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

sub_process(Handle_Mod,Node_Name,Req_Content,From,State) ->
    Check_Parameter_Fun = check_parameter,
    Request_Process_Fun = request_process,
    io:format("sub process : ~p ~p ~p ~p ~n",[Handle_Mod,Node_Name,Check_Parameter_Fun,Request_Process_Fun]),
	  Node = list_to_atom(Node_Name),
	  io:format("sub process : ~p ~n",[Node]),
    if 
			node() =/= Node -> 
				io:format("sub process : error - not for me ~p ~n",[node()]),
				gen_server:reply(From,{error,err_for_not_destnode});
			true -> 
				io:format("sub process : to do check and process ~n"),
				try 
					case apply(Handle_Mod,Check_Parameter_Fun,[Req_Content,State]) of
        		ok  ->
        			io:format("sub process : ckeck pass ~n"),
		    			Result = apply(Handle_Mod,Request_Process_Fun,[Req_Content,State]),
		    			io:format("Result output ~p",[Result]),
            	gen_server:reply(From,Result)
        	end
				catch 
        	throw:{false,Reason} ->
        		  io:format(" error : ~p ~p ~n",[Handle_Mod,Reason]),
            	gen_server:reply(From,{error,Reason});
					throw:Reason -> 
						  io:format(" error : ~p ~p ~n",[Handle_Mod,Reason]),
            	gen_server:reply(From,{error,Reason});
					_:_ ->
						  io:format(" error : ~p ~p ~n",[Handle_Mod,err_for_unknow_exception]),
							gen_server:reply(From,{error,err_for_unknow_exception}) 
    		end
		end.
	

handle_call(Request,From,State) when is_record(Request,node_req)-> 
	io:format("Recieve what Req ~p ~p ~p ~n",[Request,From,State]), %% debug info
	Node_Name= Request#node_req.node_name,
	Req_Content = Request#node_req.content,
	Handle_Mod = get_request_handle(Req_Content),
	io:format("get Handle ~p ~n",[Handle_Mod]),
  case Handle_Mod of
         unknown_cmd ->
            {reply,{error,no_such_request},State};
        _ -> 
        	  io:format("spawn a process to do ~n"), %% debug info
            _Pid = spawn(?MODULE,sub_process,[Handle_Mod,Node_Name,Req_Content,From,State]),
	          io:format("process pid :~p ~n",[_Pid]), %% debug info
            {noreply, State}
  end;
handle_call(Request,From,State) ->
	io:format("Recieve what Req ~p ~p ~p ~n",[Request,From,State]), %% debug info
  {reply,{error,err_for_request_format},State}.
%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.


%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason},State) ->
	io:format("(~p) exit with (~p)~n", [Pid, Reason]),
	%Result = ets:lookup(RunnintTaskTable, Pid),
	%case Result of 
	%	[] ->
	%		{noreply, {State, RunnintTaskTable}};
	%	{From, Request} ->
	%		gen_server:reply(From, failed)
	%end;
  {noreply,State};
handle_info(Info, State) ->
  {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
get_request_handle(Request) ->
	if 
	   	is_record(Request,vm_start_2s_req)	 				->start_vm_cmd;
	   	is_record(Request,get_node_info_2s_req)	    ->get_node_info_cmd;
      true -> unknow_cmd
    end.
