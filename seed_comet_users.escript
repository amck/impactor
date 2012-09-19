#!/usr/bin/env escript
%% -*- erlang -*-
%%! -name seed_auth@127.0.0.1 -setcookie matchpoint-dev
main(_) ->
  pong = net_adm:ping('matchpoint@127.0.0.1'),
  
  [add_user(trader_name(N), <<"password">>, fix) ||
    N <- lists:seq(1,999)].

trader_name(Num) -> 
  String = "trader_xx_" ++ integer_to_list(Num),
  list_to_binary(String).
  
add_user(UserId, Password, DeskId) ->
  io:format("Adding ~p~n", [UserId]),
  ok = rpc:call('matchpoint@127.0.0.1', user_manager, add_user, [UserId,Password,DeskId]).


