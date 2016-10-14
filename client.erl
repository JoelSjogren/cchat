-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

%% Produce initial state
initial_state(Nick, GUIName) ->
  #client_st {gui = GUIName, nick = Nick, server = none}.

%% ---------------------------------------------------------------------------

%% handle/2 handles each kind of request from GUI

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the
%% requesting process and NewState is the new state of the client.

%% Connect to server
handle(St, {connect, Server}) ->  %%TODO server_not_reached
      Data = {connect, self(), St#client_st.nick},
      io:fwrite("Client is sending: ~p~n", [Data]),
      ServerAtom = list_to_atom(Server),
      Response = genserver:request(ServerAtom, Data),
      io:fwrite("Client received: ~p~n", [Response]),
      NewServer = case Response of
        ok -> {is, ServerAtom};
        {error, _, _} -> St#client_st.server
      end,
      {reply, Response, St#client_st{server = NewServer}};

%% Disconnect from server
handle(St, disconnect) -> %%TODO server_not_reached
  case St#client_st.server of
    none ->
      {reply, {error, user_not_connected, "You must connect to a server first"}, St} ;    
    {is, Server} ->
      Data = {disconnect, self()},  %%TODO implement leave_channels_first (?)
      io:fwrite("Client is sending: ~p~n", [Data]),
      Response = genserver:request(Server, Data),
      io:fwrite("Client received: ~p~n", [Response]),
      {reply, ok, St}
  end;

% Join channel
handle(St, {join, Channel}) ->
  case St#client_st.server of
    none ->
      {reply, {error, not_connected, "You must connect to a server first."}, St} ;
    {is, Server} ->
      Data = {join, self(), Channel},
      io:fwrite("Client is sending: ~p~n", [Data]),
      Response = genserver:request(Server, Data), %TODO server checks if user already joined
      io:fwrite("Client received: ~p~n", [Response]),
      {reply, ok, St}
  end;

%% Leave channel
handle(St, {leave, Channel}) ->
  case St#client_st.server of
    none ->
      {reply, {error, not_connected, "You must connect to a server first."}, St} ;
    {is, Server} ->
      Data = {leave, self(), Channel},
      io:fwrite("Client is sending: ~p~n", [Data]),
      Response = genserver:request(Server, Data), %TODO server checks if user joined
      io:fwrite("Client received: ~p~n", [Response]),
      {reply, ok, St}
  end;

% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
  case St#client_st.server of
    none ->
      {reply, {error, not_connected, "You must connect to a server first."}, St} ;
    {is, Server} ->   
      Data = {msg_from_client, Channel, Msg, self()},
      io:fwrite("Client is sending: ~p~n", [Data]),
      Response = genserver:request(Server, Data), %TODO server checks if user joined
      io:fwrite("Client received: ~p~n", [Response]),
      {reply, ok, St}
  end;

%% Get current nick
handle(St, whoami) ->
  {reply, St#client_st.nick, St} ;

%% Set nick
handle(St = #client_st{server = MaybeServer}, {nick, Nick}) ->
  case MaybeServer of
    none -> skip;
    {is, Server} ->
      Data = {nick, self(), Nick},
      io:fwrite("Client is sending: ~p~n", [Data]),
      Response = genserver:request(Server, Data), %TODO server checks if user joined
      io:fwrite("Client received: ~p~n", [Response])
  end,
  NewSt = St#client_st{nick = Nick},
  {reply, ok, NewSt};


%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Nick, Msg}) ->
  gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Nick++"> "++Msg}),
  {reply, ok, St}.  % TODO remove _ in "gen_server"?
