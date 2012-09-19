%% Author: andrew
%% Created: Sep 19, 2012
%% Description: TODO: Add description to util
-module(protocol_util).


%%%%%%%%%%%%%%%%%%%%
%%% The Sockets.js server-side protocol as processed by https://github.com/LearnBoost/Socket.IO-node/blob/master/lib/socket.io/utils.js
%%%%%%%%%%%%%%%%%%%%
%%
%% Frame: ~m~
%% Message: some string
%% JSON Message: ~j~ ++ String Version of JSON object
%% Heartbeat: ~h~ ++ Index
%% Result: Frame ++ Length of Message ++ Frame ++ Message
-define(FRAME, "~m~").
-define(JSON_FRAME, "~j~").
-define(JSON_FRAME_LENGTH, 3).
-define(HEARTBEAT_FRAME, "~h~").
-define(HEARTBEAT_FRAME_LENGTH, 3).


%%
%% Exported Functions
%%
-export([check_for_heartbeat/1]).

%%
%% API Functions
%%
check_for_heartbeat(Str) when is_list(Str) ->
    header(Str).

%%
%% Local Functions
%%

header(?FRAME ++ Rest) ->
    header(Rest, []).
header(?FRAME ++ Rest=[_|_], Acc)->
    Length = list_to_integer(lists:reverse(Acc)),
    body(Length, Rest);
header([N|Rest], Acc) when N >= $0, N =< $9 ->
    header(Rest, [N|Acc]).

body(Length, ?JSON_FRAME++Body) ->
    false;
body(Length, ?HEARTBEAT_FRAME++Body) ->
    true ;   
body(Length, Body) ->
    false.

