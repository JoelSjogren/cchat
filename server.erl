-module(server).
-export([handle/2, initial_state/1]).
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
handle(St, {connect, Pid, Nick}) ->
  case lookup(Pid, Nick, St#server_st.clients) of
    pid_exists -> {reply, {error, user_already_connected, "You are already connected."}, St};
    nick_exists -> {reply, {error, nick_taken, "Someone else is using that nickname."}, St};
    _ ->
      Clients = dict:store(Pid, Nick, St#server_st.clients),
      {reply, ok, St#server_st{clients = Clients}}
  end;

%% Disconnect client
handle(St = #server_st{clients = Clients, channels = Channels}, {disconnect, Pid}) ->
  % Leave all channels %%TODO to be handled by client
  Forgot = fun(_Name, Pids) -> lists:member(Pid, Pids) end,
  case dict:is_empty(dict:filter(Forgot, Channels)) of
    true -> {reply, ok, St#server_st{clients = dict:erase(Pid, Clients)}};
    false -> {reply, {error, leave_channels_first, "Leave all channels before disconnecting."}, St}
  end;
  
%% Join channel
handle(St = #server_st{clients = Clients, channels = Channels}, {join, Pid, Name}) ->
  % Find the channel, creating a new process for it if necessary.
  {ChannelPid, NewChannels} = case dict:find(Name, Channels) of
    error -> 
      ChannelPid = genserver:start(no_name, initial_channel_state(Name), fun channel_handle/2),
      {ok, dict:store(Name, Pid, Channels)};
    {ok, ChannelPid} -> {ChannelPid, Channels},
  Nick = dict:fetch(Pid, clients),
  Response = genserver:request(ChannelPid, {channel_join, Pid, Nick}),
  {reply, Response, St#server_st{channels = NewChannels};

%% Set a nickname
handle(St = #server_st{clients = Clients}, {nick, Pid, Nick}) ->
  case lookup(none, Nick, Clients) of
    not_found ->
      NewClients = dict:store(Pid, Nick, Clients),
      notify = fun(ClientPid) genserver:request(ClientPid, {channel_nick_private, Pid, Nick}) end,
      lists:foreach(notify, dict:fetch_keys(clients)),
      {reply, ok, St#server_st{clients = NewClients}};
    _ ->
      {reply, {error, nick_taken, "That nickname is already in use."}, St}
  end.


%% ---------------------------------------------------------------------------

%% channel_handle/2 handles requests from clients
channel_handle(St = #channel_st{clients = Clients}, {channel_join, Pid, Nick}) ->
  {Response, NewClients} = case dict:is_key(Pid, Clients) of
    true -> {{error, user_already_joined, "You already joined this channel."}, Clients};
    false -> {ok, dict:store(Pid, Nick, Clients)}
  end
  {reply, Response, St#channel_st{clients = NewClients}};

%% Leave channel
channel_handle(St = #channel_st{clients = Clients}, {channel_leave, Pid}) ->
  case dict:is_key(Pid, Clients) of
    true ->
      NewClients = dict:erase(Pid, Clients),
      {reply, ok, St#server_st{clients = NewClients}};
    false -> {reply, {error, user_not_joined, "You are not in this channel."}, St}
  end;

%% Accept messages
channel_handle(St = #channel_st{name = Name, clients = Clients}, {msg_from_client, Msg, Pid}) ->
  case lists:member(Pid, Clients) of
    true ->
      dispatch(St, Name, Pid, Msg),
      {reply, ok, St};
    false ->
      {reply, {error, user_not_joined, "You are not in this channel."}, St}
  end;

%% Set a nickname
%% Only servers are allowed to send this kind of request.
handle(St = #channel_st{clients = Clients}, {channel_nick_private, Pid, Nick}) ->
  NewClients = dict:store(Pid, Nick, Clients),
  {reply, ok, St#server_st{clients = NewClients}};

%% ---------------------------------------------------------------------------

% Returns pid_exists if the Pid is within the registered Clients
% Returns nick_exists if the Nick provided is already assigned to a Pid
% Else, returns atom not_found
lookup(Pid, Nick, Clients) ->
  case dict:is_key(Pid, Clients) of
    true -> pid_exists;
    false ->
      case dict:is_empty(dict:filter(fun(_Pid, OldNick) -> OldNick == Nick end, Clients)) of
        false -> nick_exists;
        true -> not_found
      end
  end.

% Send a message to all other clients on the same channel
%   Pid0: pid of the sender
dispatch(St = #channel_st{clients = Clients}, Channel, Pid0, Msg) ->
  Nick = dict:fetch(Pid0, St#server_st.clients),
  SendTo = fun(Pid1) -> fun() ->
    genserver:request(Pid1, {incoming_msg, Channel, Nick, Msg})
  end end,
  [spawn_link(SendTo(Pid1)) || Pid1 <- dict:fetch_keys(Clients), Pid0 /= Pid1].


