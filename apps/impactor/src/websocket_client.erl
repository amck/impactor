%% 
%% Basic implementation of the WebSocket API:
%% http://dev.w3.org/html5/websockets/
%% However, it's not completely compliant with the WebSocket spec.
%% Specifically it doesn't handle the case where 'length' is included
%% in the TCP packet, SSL is not supported, and you don't pass a 'ws://type url to it.
%%
%% It also defines a behaviour to implement for client implementations.
%% @author Dave Bryson [http://weblog.miceda.org]
%% Modifed by Andrew McKenzie 18 Sept 12

-module(websocket_client).
-behaviour(gen_server).

%% API
-export([start/3,write/2,close/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Ready States
-define(CONNECTING,0).
-define(OPEN,1).
-define(CLOSED,2).

%% socketio protocol 
-define(MSG_FRAME, "~m~").
-define(HEARTBEAT_FRAME, "~h~").
-define(JSON_FRAME, "~j~").

-record(state, {socket,id, readystate=undefined,headers=[]}).


%% Write to the server
write(Id, Data) ->
    gen_server:cast(node_id(Id),{send,Data}).

%% Close the socket
close(Id) ->
    gen_server:cast(node_id(Id),close).

start(Id, Host,Port) ->
    gen_server:start_link({local, node_id(Id)}, ?MODULE, [{Id, Host, Port}], []).


node_id(ID)->
  farm_tools:atomize([?MODULE, '.', ID]).
 

init(Args) ->
    process_flag(trap_exit,true),
    [{Id, Host,Port}] = Args,
    {ok, Sock} = gen_tcp:connect(Host,Port,[binary,{packet, 0},{active,true}]),    
    Req = p_initial_request(Host,Port),
    ok = gen_tcp:send(Sock,Req),
    inet:setopts(Sock, [{packet, http}]),
    lager:debug("~p comet connection initialised using socket ~p",[Id, Sock]),
    {ok,#state{socket=Sock, id=Id}}.

handle_cast({send,Data}, #state{id=Id, socket=Socket} = State) ->
    lager:debug("~p sending msg ~p for comet connection with socket ~p",[Id, Data, Socket]),
    p_send(Socket, Data),    
    {noreply, State};

handle_cast(close, #state{id=Id, socket=Socket} = State) ->
    lager:debug("~p received close on socket ~p closing",[Id, Socket]),
    gen_tcp:close(State#state.socket),
    State1 = State#state{readystate=?CLOSED},
    {stop,normal,State1}.


%% Start handshake
handle_info({http,Socket,{http_response,{1,1},101,"Web Socket Protocol Handshake"}}, #state{id=Id} = State) ->
    lager:debug("~p handle_info http_response 1 1 101 web socket protocol handshake",[Id]),
    State1 = State#state{readystate=?CONNECTING,socket=Socket},
    {noreply, State1};

%% Extract the headers
handle_info({http,Socket,{http_header, _, Name, _, Value}}, State) ->
    lager:debug("~n~n getting headers",[]),
    case State#state.readystate of
	?CONNECTING ->
	    H = [{Name,Value} | State#state.headers],
	    State1 = State#state{headers=H,socket=Socket},
	    {noreply,State1};
	undefined ->
	    %% Bad state should have received response first
	    {stop,error,State}
    end;

%% Once we have all the headers check for the 'Upgrade' flag 
handle_info({http,Socket,http_eoh},#state{id=Id} = State) ->
    %% Validate headers, set state, change packet type back to raw
     case State#state.readystate of
	?CONNECTING ->
	     Headers = State#state.headers,
	     case proplists:get_value('Upgrade',Headers) of
		 X when X == "WebSocket";
            X == "websocket" ->
		      inet:setopts(Socket, [{packet, raw}]),
		      State1 = State#state{readystate=?OPEN,socket=Socket},
          % lets give the connection a little bit of time before sending the signin msgs
          % make sure things get up and running
          erlang:start_timer(1000, self(), signin),		      
		      {noreply,State1};
		 _Any  ->
		     {stop,error,State}
	     end;
	undefined ->
	    %% Bad state should have received response first
	    {stop,error,State}
    end;


%% Handshake complete, handle packets
handle_info({tcp, Socket, Data}, #state{id=Id} = State) ->
     case State#state.readystate of
	?OPEN ->    
	    D = p_unframe(binary_to_list(Data)),
      lager:debug("~p received msg ~p over socket ~p",[Id, Socket, D]),
      p_process_received_msg(Id, Socket, D),
	    {noreply,State};
	_Any ->
	    {stop,error,State}
    end;

handle_info({timeout, _Ref, signin}, #state{id=Id} = State) ->
    lager:debug("~p sending signin msg",[Id]),
    p_signin(Id),
    {noreply,State};
   
handle_info({tcp_closed, Socket},#state{id=Id} = State) ->
    lager:warning("~p received tcp_closed on socket ~p",[Id, Socket]),
    {stop,normal,State};

handle_info({tcp_error, Socket, _Reason},#state{id=Id} = State) ->
    lager:warning("~p received tcp_closed on socket ~p",[Id, Socket]),
    {stop,tcp_error,State};

handle_info({'EXIT', Pid, Reason},#state{id=Id} = State) ->
    lager:warning("~p received EXIT from Pid: ~p with Reason: ~p",[Id, Pid, Reason]),
    {noreply,State};

handle_info(Other,State) ->
    lager:debug("~n~n Received unhandled info msgs ~n~p~n~n",[Other]),
    {noreply,State}.

handle_call(_Request,_From,State) ->
    lager:debug("~n~n Received unhandled call msgs ~n~p~n~n",[_Request]),
    {reply,ok,State}.

terminate(Reason, _State) ->
    lager:debug("Terminated ~p~n",[Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

% using websockets draft-hybi-68 - very old but requires least amount of work to get running
p_initial_request(Host, Port) ->
  "GET /socket.io/websocket HTTP/1.1\r\n" ++    
  "Upgrade: WebSocket\r\n" ++
  "Connection: Upgrade\r\n" ++
  "Host: " ++ Host ++ ":" ++ integer_to_list(Port) ++ "\r\n" ++
	"Origin: http://" ++ Host ++ ":" ++ integer_to_list(Port) ++ "\r\n" ++
  "\r\n".

%% below is for websockets draft-hybi-17
%% initial_request(Host, Port) ->
%%   "GET /socket.io/websocket HTTP/1.1\r\n" ++    
%%   "Upgrade: WebSocket\r\n" ++
%%   "Connection: Upgrade\r\n" ++
%%   "Host: " ++ Host ++ ":" ++ integer_to_list(Port) ++ "\r\n" ++
%%   "Sec-Websocket-Key: x3JJHMbDL1EzLkh9GBhXDw==\r\n" ++
%%   "Sec-WebSocket-Version: 13\r\n" ++
%%   "Origin: http://" ++ Host ++ ":" ++ integer_to_list(Port) ++ "\r\n" ++
%%   "\r\n".

p_unframe([0|T]) -> p_unframe1(T).
p_unframe1([255]) -> [];
p_unframe1([H|T]) -> [H|p_unframe1(T)].

p_encode_for_socketio(Data)->
  JSON = ?JSON_FRAME ++ binary_to_list(jsx:term_to_json(Data)),
  ?MSG_FRAME ++ integer_to_list(length(JSON)) ++ ?MSG_FRAME ++ JSON.

p_send(Socket, Data)->  
  gen_tcp:send(Socket, [0] ++ p_encode_for_socketio(Data) ++ [255] ).

p_signin(Id)->
  websocket_client:write(Id, [{<<"msg">>, list_to_binary("signin" ++ Id)}]).

% all incoming msgs except for heartbeat msgs are for the time being just washed down the sink
p_process_received_msg(Id, Socket, Msg)->
  case protocol_util:check_for_heartbeat(Msg) of
    true ->
      lager:debug("~p got heartbeat ping, returning pong on socket ~p",[Id, Socket]),
      p_send(Socket, Msg);
    _->
      ok
  end.

                             
