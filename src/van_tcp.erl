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
-author('cheepeero@gmx.net').
-author('humasect@gmail.com').
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% internal
-export([acceptor_init/1]).

-include("van.hrl").
-define(SERVER, ?MODULE). 
%% -record(state, {listen_socket, acceptor, something}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% Port = ?APP_ENV(van, port),
    {ok,Port} = application:get_env(van, tcp_port),
    io:format("listening...~n"),
    case catch gen_tcp:listen(Port, ?TCP_OPTS) of
        {ok,ListenSocket} ->
            %% log here
            io:format("got socket...~n"),
            Acceptor = spawn_link(?MODULE, acceptor_init, [ListenSocket]),
            {ok, {ListenSocket,Acceptor,[]}};
        Error ->
            %% log here
            exit(Error)
    end.

terminate(_Reason, {ListenSocket,Acceptor,_}) ->
    io:format("*** should close all connections here.~n"),
    exit(Acceptor, kill),
    gen_tcp:close(ListenSocket),
    ok.

%%--------------------------------------------------------------------

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

%%--------------------------------------------------------------------

handle_info({'EXIT', _From, Reason}, {ListenSocket, _, Connections}) ->
    io:format("acceptor exit! ~p~n", [Reason]),
    NewAcceptor = spawn_link(?MODULE, acceptor_init, [ListenSocket]),
    {noreply, {ListenSocket, NewAcceptor, Connections}};

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

acceptor_init(ListenSocket) ->
    io:format("accepting...~n"),
    case gen_tcp:accept(ListenSocket) of
        {ok,Socket} ->
            gen_server:cast(?SERVER, {connected, self()}),
            io:format("start client handler!~n");
        Error ->
            exit({error,{bad_accept,Error}})
    end.


