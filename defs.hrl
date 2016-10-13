% This record defines the structure of the client process.
% Add whatever other fields you need.
% It contains the following fields:
%   gui: the name (or Pid) of the GUI process.
%   name: the nick of the client
-record(client_st, {gui, nick, server}).

% This record defines the structure of the server process.
% Add whatever other fields you need.
%   clients: [{Pid, Nick, Model}]
%   channels: [{Name, [ClientPid]}]
-record(server_st, {clients, channels}).
