%% Copyright (c) 2013 Aleksandar Radulovic <alex@a13x.net>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(aberth).

%% API.
-export([start/0, stop/0]).

-export([start_server/3]).
%% Utils
-export([no_such_module/1, not_allowed/1, not_loaded/1]).
%% Client
-export([call/4, cast/4]).


%% Types
-type handler() :: module().
-type handlers() :: [handler()].
-export_type([handlers/0]).

-type error_code() :: integer().
-type error_message() :: list() | binary().
-type error_response() :: { error,
                            { server,
                              error_code(),
                              error_message(),
                              []
                            }
                          }.

%% Aplication
start() ->
    application:ensure_all_started(aberth),
    ok.

stop() ->
	application:stop(aberth).

%% Starting and loading aberth server
-spec start_server(integer(), integer(), aberth:handlers()) -> {ok, pid()} | {error, term()}.
start_server(NbAcceptors, Port, Handlers) ->
%% @doc start aberth BERT-RPC server
%%
%% ```
%%   NbAcceptors = integer()
%%   Port = integer()
%%   Handlers - any(),
%% '''
%%
%% NbAcceptors is a number of processes that receive connections
%% Port is a port number the server should listen to
%% Handlers is a list of modules that are wired to the server
	ok = lists:foreach(fun code:ensure_loaded/1, Handlers),
	aberth_server:add_handlers(Handlers),
	ranch:start_listener(aberth, NbAcceptors, ranch_tcp,
          [{port, Port}], aberth_protocol, []).

%% Utility funs

-spec server_error(error_code(), error_message()) -> error_response().
server_error(Code, Msg) ->
	{error, {server, Code,	<<"ServerError">>, list_to_binary(Msg), []}}.

-spec no_such_module(module()) -> error_response().
no_such_module(Mod) ->
	Msg = io_lib:format("Module '~p' not found", [Mod]),
  server_error(1, Msg).

-spec not_allowed(atom()) -> error_response().
not_allowed(Func) ->
	Msg = io_lib:format("Method '~p' not allowed", [Func]),
  server_error(2, Msg).

-spec not_loaded(module()) -> error_response().
not_loaded(Mod) ->
	Msg = io_lib:format("Module '~p' not loaded", [Mod]),
  server_error(1, Msg).

%% Client API funs

call(Host, Port, {mfa, Mod, Fun, Args}, Info) when is_list(Args) andalso is_list(Info) ->
    Packets = lists:map(fun bert:encode/1, Info ++ [{call, Mod, Fun, Args}]),
    call_1(Host, Port, Packets).

cast(Host, Port, {mfa, Mod, Fun, Args}, Info) when is_list(Args) andalso is_list(Info) ->
    Packets = lists:map(fun bert:encode/1, Info ++ [{cast, Mod, Fun, Args}]),
    call_1(Host, Port, Packets).

%% Client internal funs

call_1(Host, Port, Packets) when is_list(Packets) ->
    case gen_tcp:connect(Host, Port, [binary, {packet, 4}, {active, false}]) of
        {ok, Socket} ->
            call_2(Socket, Packets);
        Error ->
            Error
    end.

call_2(Socket, Packets) ->
    ok = lists:foreach(fun(X) -> gen_tcp:send(Socket,X) end, Packets),
    case gen_tcp:recv(Socket, 0) of
        {ok, Received} ->
            gen_tcp:close(Socket),
            decode(Received);
        {error, Reason} ->
            {error, Reason}
    end.


decode(Data) ->
    case bert:decode(Data) of
        {reply, Reply} ->
            Reply;
        {noreply} ->
            ok;
        {error, Error} ->
            {error, Error};
        Other ->
            {error, {bad_response, Other}}
    end.
