-author(kanghua ).  
-email(kanghua151@msn.com).
-ifndef(comm_define_hrl).
-define(comm_define_hrl,true).


-type(os_type()      ::centos|ubuntu).
-type(vm_status()    ::running|stopped|suspend|unknown).
-type(node_status()  ::running|down|exception|unknown).
-define(PASSWORD_SIZE,12).

-record(state, {
                self,                        %% self pid
			    			nodeup_monitor_info_list,    %% mnesia table name
								module,                      %% self module name
								storage_uri
			   }).
			   
-endif.