-module(vncport).
-export([vnc_port/1]).

-include("meta_schema.hrl").
-include("cmd_shema.hrl").

vnc_port(Node_Name)->
    Vm_List = meta_data_ops:get_vms_runenv_info_by_node(Node_Name),
	  Vnc_Port = length(Vm_List)+5900,
    Vnc_Port.


