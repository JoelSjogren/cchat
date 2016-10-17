-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

% Produce initial state
initial_state(ServerName) ->
  io:format("Server ~p created.", [ServerName]),
  #server_st{clients = dict:new(), channels = dict:new()}.

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
  % Leave all channels
  Forgot = fun(_Name, Pids) -> lists:member(Pid, Pids) end,
  case dict:is_empty(dict:filter(Forgot, Channels)) of
     true -> {reply, ok, St#server_st{clients = dict:erase(Pid, Clients)}};
     false -> {reply, {error, leave_channels_first, "Leave all channels before disconnecting."}, St}
  end;
  
%% Join channel
handle(St = #server_st{channels = Channels}, {join, Pid, Name}) ->
  {Response, Pids} = case dict:find(Name, Channels) of
    error -> {ok, [Pid]};
    {ok, OldPids} ->
      case lists:member(Pid, OldPids) of
        true -> {{error, user_already_joined, "You already joined this channel."}, OldPids};
        false -> {ok, [Pid | OldPids]}
      end
    end,
  NewChannels = dict:store(Name, Pids, Channels),
  {reply, Response, St#server_st{channels = NewChannels}};

%% Leave channel
handle(St = #server_st{channels = Channels}, {leave, Pid, Channel}) ->
  case dict:find(Channel, Channels) of
    {ok, UserList} ->
      case lists:member(Pid, UserList) of
        true ->
          NewUserList = lists:delete(Pid, UserList),
          NewChannels = dict:store(Channel, NewUserList, Channels),
          {reply, ok, St#server_st{channels = NewChannels}};
        false -> {reply, {error, user_not_joined, "You are not in this channel."}, St}
      end;
    error ->
      {reply, {error, user_not_joined, "You are not in this channel."}, St}
  end;

%% Accept messages
handle(St, {msg_from_client, Channel, Msg, Pid}) ->
  Pids = dict:fetch(Channel, St#server_st.channels),
  case lists:member(Pid, Pids) of
    true ->
      dispatch(St, Channel, Pid, Msg),
      {reply, ok, St};
    false ->
      {reply, {error, user_not_joined, "You are not in this channel."}, St}
  end;

%% Set a nickname
handle(St = #server_st{clients = Clients}, {nick, Pid, Nick}) ->
  case lookup(none, Nick, Clients) of
    not_found ->
      NewClients = dict:store(Pid, Nick, Clients),
      {reply, ok, St#server_st{clients = NewClients}};
    _ ->
      {reply, {error, nick_taken, "That nickname is already in use."}, St}
  end.

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
dispatch(St = #server_st{channels = Channels}, Channel, Pid0, Msg) ->
  Nick = dict:fetch(Pid0, St#server_st.clients),
  SendTo = fun(Pid1) -> fun() ->
    genserver:request(Pid1, {incoming_msg, Channel, Nick, Msg})
  end end,
  Recipients = dict:fetch(Channel, Channels),
  [spawn_link(SendTo(Pid1)) || Pid1 <- Recipients, Pid0 /= Pid1].
  
