% This record defines the structure of the client process.
% Add whatever other fields you need.
% It contains the following fields:
%   gui: the name (or Pid) of the GUI process.
%   nick: of the client
%   maybe_server: the 'none' atom or {is, pid()}
%   channels: dict(name(), pid())
-record(client_st, {gui, nick, maybe_server, channels}).

% This record defines the structure of the server process.
% Add whatever other fields you need.
%   clients: dict(pid(), nick())
%   channels: dict(name(), pid())
-record(server_st, {clients, channels}).

%   name: of the channel
%   clients: dict(pid(), nick())  (a subset of the server clients)
-record(channel_st, {name, clients}).
