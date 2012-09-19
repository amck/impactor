-module(comet).
-behaviour(gen_server).

-export([fire/1, count/0, holler/2, kill/1]).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {web_service_host, web_service_port, conns=0, trader_name }).

fire(NumConnections)->
  gen_server:call(?MODULE, {fire,NumConnections}).

count()->
  gen_server:call(?MODULE, {count}).

holler(Id, Msg)->
  websocket_client:write(Id, Msg).

kill(Id)->
  websocket_client:close(Id).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  process_flag(trap_exit, true),  
  HostIp =
    case application:get_env(impactor, web_service_host) of
        {ok, Ip} ->
          Ip;
        _ ->
          "localhost"
    end,

  HostPort =
    case application:get_env(impactor, web_service_port) of
        {ok, Port} ->
          Port;
        _ ->
          9001
    end,

  TraderName =
    case application:get_env(impactor, trader_name) of
        {ok, Name} ->
          Name;
        _ ->
          "trader_xx"
    end,  
  {ok,#state{web_service_host=HostIp, web_service_port=HostPort, trader_name=TraderName}}.

handle_call({fire, NumNewConns}, _From, #state{web_service_host=HostIp, web_service_port=HostPort, conns=CurConnCount, trader_name=TraderName} = State) ->
    {ok, ActiveConns} = p_fire_connections(NumNewConns, CurConnCount, HostIp, HostPort, TraderName),
    lager:info("Fired ~p comet connections at target",[ActiveConns]),
    {reply, ok, State#state{conns = ActiveConns}};

handle_call({count}, _From, #state{conns=CurConnCount} = State) ->
    Reply = integer_to_list(CurConnCount) ++ " active comet connections",
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', From, Reason}, #state{conns=CurConnCount} = State) ->
  lager:warning("Comet connection process died!! PID ~p,  Reason:~p",[From, Reason]),
  lager:warning("~p connections maintaining attack",[CurConnCount -1]),
  {noreply, State#state{conns = CurConnCount-1}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

p_fire_connections(ConnsToFire, CurConnCount, HostIp, HostPort, TraderName)->
  
  [ websocket_client:start(p_make_trader_name(N, TraderName), HostIp, HostPort) ||
    N <- lists:seq(CurConnCount,CurConnCount + ConnsToFire - 1)],
  {ok, CurConnCount + ConnsToFire}.


%% p_fire_connections(CurConnCount, NumNewConns, HostIp, HostPort, TraderName)->
%%   p_fire_connections(CurConnCount, CurConnCount, NumNewConns, HostIp, HostPort, TraderName).
%% p_fire_connections(CurConnCount, NewConnNum, NumNewConns, HostIp, HostPort, TraderName) when NewConnNum > (NumNewConns + CurConnCount)->
%%   {ok, CurConnCount+NewConnNum};
%% p_fire_connections(CurConnCount, NewConnNum, NumNewConns, HostIp, HostPort, TraderName) ->
%%   {ok,Pid} = websocket_client:start(p_make_trader_name(CurConnCount + NewConnNum, TraderName), HostIp, HostPort),
%%   p_fire_connections(CurConnCount, NewConnNum+1, NumNewConns, HostIp, HostPort, TraderName).

p_make_trader_name(Num, TraderName)->
  TraderName ++ "_" ++ integer_to_list(Num).
