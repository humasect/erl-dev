%%%-------------------------------------------------------------------
%%% @author Lyndon Tremblay <humasect@gmail.com>
%%% @copyright (C) 2006, Ciprian Ciubotariu
%%% @copyright (C) 2010, Lyndon Tremblay
%%% @doc
%%%
%%% The network server accepts connections on the port specified by
%%% the application's <code>server_port</code> setting. When a
%%% connection is made the netserver starts a client handler by calling
%%% the @link{clienthandler:start/1} function and resumes accepting
%%% connections.
%%% See the documentation of module @link{clienthandler} for details
%%% on how the connection is handled.
%%%
%%% @end
%%% Created : 17 Sep 2010 by Lyndon Tremblay <humasect@gmail.com>
%%%-------------------------------------------------------------------
-module(van_tcp).
-behaviour(gen_server).
-author('cheepeero@gmx.net').
-author('humasect@gmail.com').

%% API
-export([start_link/0]).

%% callbacks
-export([init/1, terminate/2,
         handle_call/3, handle_cast/2, handle_info/2,
         code_change/3]).

%% internal
-export([acceptor_init/1]).

-include("van.hrl").
-define(SERVER, ?MODULE). 
%% -record(state, {listen_socket, acceptor, something}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    {ok,Port} = application:get_env(van, tcp_port),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-define(spawn_acceptor, spawn_link(?MODULE, acceptor_init, [ListenSocket])).

init([Port]) ->
    case gen_tcp:listen(Port, ?TCP_OPTS) of
        {ok,ListenSocket} ->
            %% log here
            io:format("got socket...~n"),
            {ok, {ListenSocket, ?spawn_acceptor, []}};
        Error ->
            exit(Error)
    end.

terminate(_Reason, {ListenSocket, Acceptor, _Clients}) ->
    io:format("** acceptor terminated. Should kill all clients.~n"),
    exit(Acceptor, kill),
    gen_tcp:close(ListenSocket),
    ok.

handle_call(_Call, _From, State) ->
    {reply, ok, State}.

handle_cast({add_client, Pid}, {ListenSocket,Acceptor,Clients}) ->
    unlink(Acceptor),
    {noreply, {ListenSocket, ?spawn_acceptor, [Pid | Clients]}}.

handle_info({'EXIT', _Pid, Reason}, {ListenSocket,_,Clients}) ->
    io:format("acceptor died! ~p~n", [Reason]),
    {noreply, {ListenSocket, ?spawn_acceptor, Clients}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

acceptor_init(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok,Socket} ->
            io:format("~w connected.~n", [Socket]),
            gen_server:cast(?SERVER, {add_client, self()}),
            van_client:loop({tcp, Socket, waiting_auth});
            %% supervisor:start_child...
            %%?MODULE:accept_loop(ListenSocket);
        Else ->
            io:format("*** accept returned ~w.", [Else]),
            exit({error, {bad_accept, Else}})
    end.
