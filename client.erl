-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

%% Produce initial state
initial_state(Nick, GUIName) ->
  #client_st {gui = GUIName, nick = Nick, maybe_server = none, channels = dict:new()}.

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
handle(St = #client_st{nick = Nick}, {connect, ServerName}) ->
  ServerAtom = list_to_atom(ServerName),
  case lists:member(ServerAtom, registered()) of
    false ->
      {reply, {error, server_not_reached, "Could not reach such server (not registered)"}, St};
    true ->
      Data = {connect, self(), Nick},
      try genserver:request(ServerAtom, Data) of
        ok -> {reply, ok, St#client_st{maybe_server = {is, ServerAtom}}};
        Error -> {reply, Error, St}
      catch
        _:_ -> {reply, {error, server_not_reached, "Could not reach such server (timeout)"}, St}
      end
  end;

%% Disconnect from server
handle(St = #client_st{maybe_server = MaybeServer, channels = Channels}, disconnect) ->
  case {MaybeServer, dict:is_empty(Channels)} of
    {none, _} ->
      {reply, {error, user_not_connected, "You must connect to a server first"}, St} ;    
    {_, false} ->
      {reply, {error, leave_channels_first, "Leave all channels before disconnecting."}, St};
    {{is, Server}, true} ->
      % Tell server 
      Data = {disconnect, self()},
      try genserver:request(Server, Data) of
        ok -> {reply, ok, St#client_st{maybe_server = none}}
      catch
        _:_ -> {reply, {error, server_not_reached, "Could not reach such server (timeout)"}, St}
      end
  end;

% Join channel
handle(St = #client_st{maybe_server = MaybeServer, channels = Channels}, {join, Name}) ->
  case MaybeServer of
    none ->
      {reply, {error, not_connected, "You must connect to a server first."}, St} ;
    {is, Server} ->
      Data = {join, self(), Name},
      case genserver:request(Server, Data) of
        {ok, ChannelPid} ->
          NewChannels = dict:store(Name, ChannelPid, Channels),
          {reply, ok, St#client_st{channels = NewChannels}};
        Error ->
          {reply, Error, St}
      end
  end;

%% Leave channel
handle(St = #client_st{maybe_server = MaybeServer, channels = Channels}, {leave, Name}) ->
  case {MaybeServer, dict:find(Name, Channels)} of
    {none, _} ->
      {reply, {error, not_connected, "You must connect to a server first."}, St} ;
    {_, error} ->
      {reply, {error, user_not_joined, "You are not in this channel."}, St};
    {{is, _Server}, {ok, ChannelPid}} ->
      Data = {channel_leave, self()},
      case genserver:request(ChannelPid, Data) of
        ok ->
          NewChannels = dict:erase(Name, Channels),
          {reply, ok, St#client_st{channels = NewChannels}};
        Error ->
          {reply, Error, St}
      end
      
  end;

% Sending messages
handle(St = #client_st{channels = Channels}, {msg_from_GUI, Name, Msg}) ->
  case dict:find(Name, Channels) of
    error -> {reply, {error, user_not_joined, "You must connect to the channel first."}, St} ;
    {ok, ChannelPid} ->
      Data = {msg_from_client, Msg, self()},
      Response = genserver:request(ChannelPid, Data),
      {reply, Response, St}
  end;

%% Get current nick
handle(St = #client_st{nick = Nick}, whoami) ->
  {reply, Nick, St} ;

%% Set nick
handle(St = #client_st{maybe_server = MaybeServer, channels = Channels}, {nick, Nick}) ->
  case MaybeServer of
    none -> Response = ok;
    {is, Server} ->
      ChannelPids = [Pid || {_, Pid} <- dict:to_list(Channels)],
      Data = {nick, self(), Nick, ChannelPids},
      Response = genserver:request(Server, Data)
  end,
  NewSt = St#client_st{nick = Nick},
  {reply, Response, NewSt};

%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Nick, Msg}) ->
  gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Nick++"> "++Msg}),
  {reply, ok, St}.
