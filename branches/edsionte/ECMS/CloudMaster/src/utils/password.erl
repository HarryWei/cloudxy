
-module(password).


%%
%% Include files
%%

%%
%% Exported Functions
%%

-export([random_password/1]).

%%
%% API Functions
%%

random_password(Len) ->
    Chrs = list_to_tuple("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"),
    F = fun(_, R) -> [element(random:uniform(size(Chrs)), Chrs) | R] end,
    lists:foldl(F, "", lists:seq(1, Len)).