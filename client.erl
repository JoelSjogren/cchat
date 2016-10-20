-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

%% Produce initial state
initial_state(Nick, GUIName) ->
  #client_st {gui = GUIName, nick = Nick, server = none}.

%% ---------------------------------------------------------------------------

%% handle/2 handles each kind of request from GUI

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the
%% requesting process and NewState is the new state of the client.

%% Common variable names:
%%  Pid: of a client
%%  Nick: a nickname
%%  Name or Channel: name of a channel
%%  New~: updated value of ~
%%  Msg: a string

%% Connect to server
handle(St, {connect, Server}) ->
  ServerAtom = list_to_atom(Server),
  case lists:member(ServerAtom, registered()) of
    false ->
      {reply, {error, server_not_reached, "Could not reach such server (not registered)"}, St};
    true ->
      Data = {connect, self(), St#client_st.nick},
      try genserver:request(ServerAtom, Data) of
        ok -> {reply, ok, St#client_st{server = {is, ServerAtom}}};
        Error -> {reply, Error, St}
      catch
        _:_ -> {reply, {error, server_not_reached, "Could not reach such server (timeout)"}, St}
      end
  end;

%% Disconnect from server
handle(St, disconnect) ->
  case St#client_st.server of
    none ->
      {reply, {error, user_not_connected, "You must connect to a server first"}, St} ;    
    {is, Server} ->
      Data = {disconnect, self()},
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
      Response = genserver:request(Server, Data),
      {reply, Response, St}
  end;

%% Leave channel
handle(St, {leave, Channel}) ->
  case St#client_st.server of
    none ->
      {reply, {error, not_connected, "You must connect to a server first."}, St} ;
    {is, Server} ->
      Data = {leave, self(), Channel},
      Response = genserver:request(Server, Data),
      {reply, Response, St}
  end;

% Sending messages
handle(St = #client_st{channels = Channels}, {msg_from_GUI, Channel, Msg}) ->
  case dict:find(Channel, Channels) of
    error -> {reply, {error, user_not_joined, "You must connect to the channel first."}, St} ;
    {ok, Pid} ->
      Data = {msg_from_client, Channel, Msg, self()},
      Response = genserver:request(Pid, Data),
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
      Response = genserver:request(Server, Data)
  end,
  NewSt = St#client_st{nick = Nick},
  {reply, Response, NewSt};

%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Nick, Msg}) ->
  gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Nick++"> "++Msg}),
  {reply, ok, St}.
