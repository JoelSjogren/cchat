-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ServerName) ->
  io:format("Server `~p' created.", [ServerName]),
  #server_st{clients = dict:new(), channels = dict:new()}.

%% ---------------------------------------------------------------------------

%% handle/2 handles requests from clients

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the client
%% and NewState is the new state of the server.

%% Connect client
handle(St, {connect, Pid, Nick}) ->
  case lookup(Pid, Nick, St#server_st.clients) of
    pid_exists -> {reply, {error, user_already_connected, "You are already connected."}, St};
    nick_exists -> {reply, {error, nick_taken, "Someone else is using that nickname."}, St};
    _ ->
      %Model = spawn_link(fun() -> client_model(Pid, Nick) end),
      Clients = dict:store(Pid, Nick, St#server_st.clients),
      {reply, ok, St#server_st{clients = Clients}}
  end;

%% Disconnect client
handle(St, {disconnect, Pid}) ->
  Clients = dict:erase(Pid, St#server_st.clients),
  % Leave all channels
  Channels = dict:map(fun(_, V) -> lists:filter(fun(X) -> X /= Pid end, V) end),
  {reply, ok, #server_st{clients = Clients, channels = Channels}};
  

handle(St, {join, Pid, Name}) ->
  Pids = case dict:find(Name, St#server_st.channels) of
    error -> [Pid];
    {ok, OldPids} -> [Pid | OldPids]
  end,
  Channels = dict:store(Name, Pids, St#server_st.channels),
  {reply, ok, St#server_st{channels = Channels}};

handle(St, {leave, Pid, Channel}) ->
  NewUserList = [X || X <- dict:find(Channel, St#server_st.channels), X /= Pid],
  NewChannels = dict:store(Channel, NewUserList, St#server_st.channels),
  {reply, ok, St#server_st{channels = NewChannels}};

handle(St, Request) ->
  io:fwrite("Server received: ~p~n", [Request]),
  Response = "hi!",
  io:fwrite("Server is sending: ~p~n", [Response]),
  {reply, Response, St}.

lookup(Pid, Nick, Clients) ->
  case dict:is_key(Pid, Clients) of
    true -> pid_exists;
    false ->
      case dict:is_empty(dict:filter(fun(_Pid, OldNick) -> OldNick == Nick end, Clients)) of
        false -> nick_exists;
        true -> not_found
      end
  end.
