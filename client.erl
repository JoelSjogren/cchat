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
handle(St, {connect, Server}) ->
  Data = {connect, self(), St#client_st.nick},
  io:fwrite("Client is sending: ~p~n", [Data]),
  ServerAtom = list_to_atom(Server),
  case lists:member(ServerAtom, registered()) of
    false ->
      {reply, {error, server_not_reached, "Could not reach such server (not registered)"}, St};
    true ->
      try genserver:request(ServerAtom, Data) of
        ok -> {reply, ok, St#client_st{server = {is, ServerAtom}}};
        Error -> {reply, Error, St}
      catch
        _:_ -> {reply, {error, server_not_reached, "Could not reach such server (timeout)"}, St}
      end
  %io:fwrite("Client received: ~p~n", [Response]),
  end;

%% Disconnect from server
handle(St, disconnect) ->
  case St#client_st.server of
    none ->
      {reply, {error, user_not_connected, "You must connect to a server first"}, St} ;    
    {is, Server} ->
      Data = {disconnect, self()},
      io:fwrite("Client is sending: ~p~n", [Data]),
      try genserver:request(Server, Data) of
        ok -> {reply, ok, St#client_st{server = none}};
        Error -> {reply, Error, St}
      catch
        _:_ -> {reply, {error, server_not_reached, "Could not reach such server (timeout)"}, St}
      end
  end;

% Join channel
handle(St, {join, Channel}) ->
  case St#client_st.server of
    none ->
      {reply, {error, not_connected, "You must connect to a server first."}, St} ;
    {is, Server} ->
      Data = {join, self(), Channel},
      io:fwrite("Client is sending: ~p~n", [Data]),
      Response = genserver:request(Server, Data),
      io:fwrite("Client received: ~p~n", [Response]),
      {reply, Response, St}
  end;

%% Leave channel
handle(St, {leave, Channel}) ->
  case St#client_st.server of
    none ->
      {reply, {error, not_connected, "You must connect to a server first."}, St} ;
    {is, Server} ->
      Data = {leave, self(), Channel},
      io:fwrite("Client is sending: ~p~n", [Data]),
      Response = genserver:request(Server, Data),
      io:fwrite("Client received: ~p~n", [Response]),
      {reply, Response, St}
  end;

% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
  case St#client_st.server of
    none ->
      {reply, {error, not_connected, "You must connect to a server first."}, St} ;
    {is, Server} ->   
      Data = {msg_from_client, Channel, Msg, self()},
      io:fwrite("Client is sending: ~p~n", [Data]),
      Response = genserver:request(Server, Data),
      io:fwrite("Client received: ~p~n", [Response]),
      {reply, Response, St}
  end;

%% Get current nick
handle(St, whoami) ->
  {reply, St#client_st.nick, St} ;

%% Set nick
handle(St = #client_st{server = MaybeServer}, {nick, Nick}) ->
  case MaybeServer of
    none -> Response = ok;
    {is, Server} ->
      Data = {nick, self(), Nick},
      io:fwrite("Client is sending: ~p~n", [Data]),
      Response = genserver:request(Server, Data),
      io:fwrite("Client received: ~p~n", [Response])
  end,
  NewSt = St#client_st{nick = Nick},
  {reply, Response, NewSt};


%% Incoming message
handle(St = #client_st { gui = GUIName }, Data = {incoming_msg, Channel, Nick, Msg}) ->
  io:fwrite("Server sent (to ~p): ~p~n", [GUIName, Data]),
  gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Nick++"> "++Msg}),
  io:fwrite("Displayed it.~n"),
  {reply, ok, St}.
