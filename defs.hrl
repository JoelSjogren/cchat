% This record defines the structure of the client process.
% Add whatever other fields you need.
% It contains the following fields:
%   gui: the name (or Pid) of the GUI process.
%   name: the nick of the client
-record(client_st, {gui, nick, server}).

%-record(client_info, {nick}).

% This record defines the structure of the server process.
% Add whatever other fields you need.
%   clients: dict(pid(), nick())
%   channels: dict(name(), [pid()])
-record(server_st, {clients, channels}).
