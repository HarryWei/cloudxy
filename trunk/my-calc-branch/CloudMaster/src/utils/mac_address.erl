%% Author: Sunny
%% Created: 2011-08-02
%% Description: get Xen mac
-module(mac_address).
-date("2011.08.02").  

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([get_one_mac/1]).

%%
%% API Functions
%%
get_one_mac(Data) ->
	Digest = erlang:md5(Data),
	Md5_list = lists:sublist(binary_to_list(Digest), 3),
	Result =   list_to_hex(Md5_list),
	[Mac1|Leaving1] = Result,
	[Mac2|Leaving2] = Leaving1,
	[Mac3|_] = Leaving2,
	Mac = lists:concat(["00:16:3E:", Mac1, ":", Mac2, ":",  Mac3]),
	Mac.

%%
%% Local Functions
%%
list_to_hex(L) ->
	lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(X) when X < 256 ->
	[hex(X div 16), hex(X rem 16)].
hex(N) when N < 10 ->
	$0+N;
hex(N) when N >= 10, N < 16 ->
	$A+(N-10).

