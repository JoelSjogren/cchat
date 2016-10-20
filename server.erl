-module(server).
-export([handle/2, initial_state/1, channel_handle/2]).
-include_lib("./defs.hrl").

% Produce initial state
initial_state(ServerName) ->
  io:format("Server ~p created.", [ServerName]),
  #server_st{clients = dict:new(), channels = dict:new()}.

% Produce initial channel state
initial_channel_state(ChannelName) ->
  io:format("Channel ~p created.", [ChannelName]),
  #channel_st{name = ChannelName, clients = dict:new()}.

%% ---------------------------------------------------------------------------

%% handle/2 handles requests from clients

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the client
%% and NewState is the new state of the server.

%% Common variable names:
%%  Pid: of a client
%%  Nick: a nickname
%%  Name or Channel: name of a channel
%%  New~: updated value of ~
%%  Msg: a string

%% Connect client
handle(St = #server_st{clients = Clients}, {connect, ClientPid, Nick}) ->
  case lookup(ClientPid, Nick, Clients) of
    pid_exists -> {reply, {error, user_already_connected, "You are already connected."}, St};
    nick_exists -> {reply, {error, nick_taken, "Someone else is using that nickname."}, St};
    _ ->
      NewClients = dict:store(ClientPid, Nick, Clients),
      {reply, ok, St#server_st{clients = NewClients}}
  end;

%% Disconnect client
% Assumes that the user has left all channels
handle(St = #server_st{clients = Clients}, {disconnect, ClientPid}) ->
  {reply, ok, St#server_st{clients = dict:erase(ClientPid, Clients)}};
  
%% Join channel
handle(St = #server_st{channels = Channels, clients = Clients}, {join, ClientPid, Name}) ->
  % Find the channel, creating a new process for it if necessary.
  {ChannelPid, NewChannels} = case dict:find(Name, Channels) of
    error -> 
      FreshChannelPid = genserver:start(no_name, initial_channel_state(Name), fun channel_handle/2),
      {FreshChannelPid, dict:store(Name, FreshChannelPid, Channels)};
    {ok, ExistingChannelPid} -> {ExistingChannelPid, Channels}
  end,
  Nick = dict:fetch(ClientPid, Clients),
  Response = genserver:request(ChannelPid, {channel_join, ClientPid, Nick}),
  {reply, Response, St#server_st{channels = NewChannels}};

%% Set a nickname
% (The AffectedChannels are sent for the sake of efficiency.)
handle(St = #server_st{clients = Clients},
       {nick, ClientPid, Nick, AffectedChannelPids}) ->
  case lookup(none, Nick, Clients) of
    not_found ->
      NewClients = dict:store(ClientPid, Nick, Clients),
      Notify = fun(ChannelPid) ->
        genserver:request(ChannelPid, {channel_nick_private, ClientPid, Nick})
      end,
      [Notify(ChannelPid) || ChannelPid <- AffectedChannelPids],% a bit slower: dict:to_list(Channels)],
      %[spawn_link(Notify(ChannelPid)) || ChannelPid <- dict:fetch_keys(Channels)],  % Don't ever do this!
      {reply, ok, St#server_st{clients = NewClients}};
    _ ->
      {reply, {error, nick_taken, "That nickname is already in use."}, St}
  end.


%% ---------------------------------------------------------------------------

%% channel_handle/2 handles requests from clients

%% Join channel
% Only servers are allowed to send this kind of request.
channel_handle(St = #channel_st{clients = Clients}, {channel_join, ClientPid, Nick}) ->
  {Response, NewClients} = case dict:is_key(ClientPid, Clients) of
    true -> {{error, user_already_joined, "You already joined this channel."}, Clients};
    false -> {{ok, self()}, dict:store(ClientPid, Nick, Clients)}
  end,
  {reply, Response, St#channel_st{clients = NewClients}};

%% Leave channel
% Assumes that the client is connected to the channel
channel_handle(St = #channel_st{clients = Clients}, {channel_leave, ClientPid}) ->
  case dict:is_key(ClientPid, Clients) of
    true ->
      NewClients = dict:erase(ClientPid, Clients),
      {reply, ok, St#channel_st{clients = NewClients}};
    false -> {reply, {error, user_not_joined, "You are not in this channel."}, St}
  end;

%% Accept messages
channel_handle(St = #channel_st{name = Name, clients = Clients}, {msg_from_client, Msg, ClientPid}) ->
  case dict:is_key(ClientPid, Clients) of
    true ->
      dispatch(Clients, Name, ClientPid, Msg),
      {reply, ok, St};
    false ->
      {reply, {error, user_not_joined, "You are not in this channel."}, St}
  end;

%% Set a nickname
% Only servers are allowed to send this kind of request.
channel_handle(St = #channel_st{clients = Clients}, {channel_nick_private, ClientPid, Nick}) ->
  case dict:is_key(ClientPid, Clients) of
    true ->
      NewClients = dict:store(ClientPid, Nick, Clients),
      {reply, ok, St#channel_st{clients = NewClients}};
    false ->
      {reply, ok, St}
  end.

%% ---------------------------------------------------------------------------

% Returns pid_exists if the Pid is within the registered Clients
% Returns nick_exists if the Nick provided is already assigned to a Pid
% Else, returns atom not_found
lookup(ClientPid, Nick, Clients) ->
  case dict:is_key(ClientPid, Clients) of
    true -> pid_exists;
    false ->
      case dict:is_empty(dict:filter(fun(_Pid, OldNick) -> OldNick == Nick end, Clients)) of
        false -> nick_exists;
        true -> not_found
      end
  end.

% Send a message to all other clients on the same channel
%   ClientPid0: pid of the sender
dispatch(Clients, Channel, ClientPid0, Msg) ->
  Nick = dict:fetch(ClientPid0, Clients),
  SendTo = fun(ClientPid1) -> fun() ->
    genserver:request(ClientPid1, {incoming_msg, Channel, Nick, Msg})
  end end,
  [spawn_link(SendTo(ClientPid1)) || ClientPid1 <- dict:fetch_keys(Clients), ClientPid0 /= ClientPid1].


