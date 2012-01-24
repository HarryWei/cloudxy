%%% -------------------------------------------------------------------
%%% Author  : 77
%%% Description :
%%% Created : 
%%% -------------------------------------------------------------------
-module(meta_server).
-author(kanghua).  
-email(kanghua151@msn.com).  
 -behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("meta_schema.hrl").
-include("cmd_shema.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start_link/0]).
%% gen_server callbacks
-export([init/1, sub_process/4, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {
                self,                        %% self pid
			    			nodeup_monitor_info_list,    %% mnesia table name
								module                       %% self module name
			   }).

%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
	io:format("----- start meta server -------~n"), 
        gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

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
	io:format("----- meta server init  ------- ~n") , 
	process_flag(trap_exit,true),  
	monitor_nodes(),          %% monitor node down or node up
	{ok,Nodes} = ensure_schema(),
	init_meta_tables(Nodes),        %% wait for sync mneisa
	io:format("----- to create sequence  ------- ~n") , 
	sequence:create_sequence(Nodes),
	%io:format("----- to init sequence  ------- ~n") , 
	%sequence:init_sequence(vm_id,0),
  %sequence:init_sequence(vdisk_id,0),
	%% read node info from resource table.
	%% start all node.
  {ok, #state{
				self=self(),
				nodeup_monitor_info_list=[], %nodeup_monitor_dict = dict:new()
        module = ?MODULE
			  }}.

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

sub_process(Handle_Mod,Request,From,State) ->
  Check_Parameter_Fun = check_parameter,
  Request_Process_Fun = request_process,
  io:format("sub process : ~p ~p ~p ~n",[Handle_Mod,Check_Parameter_Fun,Request_Process_Fun]),
	try 
		case apply(Handle_Mod,Check_Parameter_Fun,[Request,State]) of 
        ok  ->
        	  io:format(" check pass , to do process : ~p ~p ~n",[Handle_Mod,request_process]),
		    		Result = apply(Handle_Mod,Request_Process_Fun,[Request,State]),
		    		io:format(" Result ~p",[Result]),
            gen_server:reply(From,Result)
    end
	catch
    throw:{false,Reason} -> 
    				io:format(" error : ~p ~p ~n",[Handle_Mod,Reason]),
            gen_server:reply(From,{error,Reason});
		throw:Reason -> 
					  io:format(" error :~p ~p ~n",[Handle_Mod,Reason]),
            gen_server:reply(From,{error,Reason});
		_:_ ->
			io:format(" error : ~p ~p ~n",[Handle_Mod,err_for_unknown_exception]),
			gen_server:reply(From,{error,err_for_unknown_exception})
  end.

handle_call(Request,From,State) -> 
	io:format("Recieve what Req ~p ~p ~p ~n",[Request,From,State] ), %% debug info
	Handle_Mod  = get_request_handle(Request),
	io:format("get Handle ~p ~n",[Handle_Mod]),
  case Handle_Mod of
        unknown_cmd ->
            {reply,{error,no_such_request},State};
        _ -> 
        	  io:format("spawn a process to do ~n"), %% debug info
            _Pid = spawn(?MODULE,sub_process,[Handle_Mod,Request,From,State]),
	          io:format("process pid :~p ~n",[_Pid]), %% debug info
            {noreply, State}
  end.
 

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

handle_info({nodeup, Node}, State) ->
	io:format("node up !!"),
	%% restart node controller 
	case rpc:call(Node,application,start,[node_controller],1000) of
		 {badrpc,{Reason,_}} when Reason == already_started ->
					io:format("node come back ~p ~n",[Node]);
		 {badrpc,Reason}  ->
					io:format("start node controller fail ! reason ~p ~n",[Reason]);
	     Value -> void
    end,
    {noreply, State};
handle_info({nodedown, Node}, State) ->
	io:format("node down !!"),
    %% modify node status
   meta_data_ops:open_node_resource(Node),
	%% start a ping to connect period node.
	Pid = spawn_link(fun()->nodeup_monitor(Node) end),
    %% record node & pid pair 
	List = State#state.nodeup_monitor_info_list,
	NewState = State#state{nodeup_monitor_info_list=[{Node,Pid}|List]},
    {noreply, NewState};
handle_info({'EXIT', Pid, Reason}, State) ->
	List = State#state.nodeup_monitor_info_list,
    NewState = State#state{nodeup_monitor_info_list=lists:keydelete(Pid,2,List)},
	{noreply, NewState}.

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
init_meta_tables(Nodes) ->
    mnesia:stop(),
    mnesia:start(),
    meta_data_ops:init_table_netaddr_resource(Nodes)  orelse throw({error,err_for_init_table}),
    meta_data_ops:init_table_node_resource(Nodes)     orelse throw({error,err_for_init_table}),
    meta_data_ops:init_table_vm_resource_config(Nodes)     orelse throw({error,err_for_init_table}),
    meta_data_ops:init_table_vm_runenv_info(Nodes)     orelse throw({error,err_for_init_table}),
    meta_data_ops:init_table_vdisk_info(Nodes)    orelse throw({error,err_for_init_table}),
    ok = mnesia:wait_for_tables([netaddr_resource,node_resource,vm_resource_config,vm_runenv_info,vdisk_info], 2000).

ensure_schema() ->
	  DefaultMetaNodes = [node()],
	  Nodes = case application:get_env(cloud_master,meta_nodes) of
					{ok, Value} -> 
			   			io:format("get meta nodes ~p !! ~n",[Value]),
			   			Value;
				  undefined ->
				 			io:format("not define meta nodes, use self!! ~n"),
				 			DefaultMetaNodes
					end,
	io:format("Meta Nodes list ~p ~n",Nodes),
  case mnesia:create_schema(Nodes) of
    ok -> 
    	 io:format("schema create ok"),
    	 {ok,Nodes};
    {error, {Node, {already_exists, Node}}} -> 
    	 io:format("schema has create in ~p ~n",[Node]),
    	 {ok,Nodes};
    Error -> 
    	 io:format("schema create error ~p ~n",[Error]),
    	 Error
  end.

%%
monitor_nodes()->
	case net_kernel:monitor_nodes(true) of 
		ok -> ok;
		_Error -> 
			io:format("unable to start monitor") ,
			{error,err_for_monitor_nods}
	end. 
nodeup_monitor(Node) ->
     	 case net_adm:ping(Node) of
		 	pong ->
			 	io:format("node up !! ~n"),
				ok ;
		 	pang ->
			 	io:format("node still down ~n"),
				receive
					exit ->
						void
				after 3000 ->
						nodeup_monitor(Node)
				end
		 end.
%%
get_request_handle(Request) ->
	if 
	  is_record(Request,add_node_2m_req)	 		->add_node_cmd;
    is_record(Request,drop_node_2m_req)			->drop_node_cmd;
    is_record(Request,get_node_info_2m_req)		->get_node_info_cmd;
    is_record(Request,get_vm_info_2m_req)		->get_vm_info_cmd;
   	is_record(Request,get_node_status_2m_req)	->get_node_status_cmd;
		is_record(Request,get_vm_status_2m_req)		->get_vm_status_cmd;
		is_record(Request,vm_create_2m_req)			->create_vm_cmd;
		is_record(Request,vm_start_2m_req)			->start_vm_cmd;
		is_record(Request,vm_stop_2m_req)			->stop_vm_cmd;
		is_record(Request,vm_destroy_2m_req)		->destroy_vm_cmd;
		is_record(Request,vdisk_create_2m_req)		->create_vdisk_cmd;
 		is_record(Request,vdisk_attach_2m_req)		->attach_vdisk_cmd;
 		is_record(Request,vdisk_deattach_2m_req)	->deattach_vdisk_cmd;
		is_record(Request,vdisk_destroy_2m_req)		->destroy_vdisk_cmd;
        true -> unknown_cmd
    end.
