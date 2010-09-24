%%%-------------------------------------------------------------------
%%% @author Lyndon Tremblay <humasect@gmail.com>
%%% @copyright (C) 2010, Lyndon Tremblay
%%%            (C) 2006, Ciprian Ciubotariu
%%% @doc
%%%
%%% The network server accepts connections on the port specified by
%%% the application's <code>tcp_port</code> setting. When a
%%% connection is made, starts a client handler (game) and resumes
%%% accepting connections.
%%% See the documentation of module @link{} for details
%%% on how the connection is handled.
%%%
%%% @end
%%% Created : 17 Sep 2010 by Lyndon Tremblay <humasect@gmail.com>
%%%-------------------------------------------------------------------
-module(zen_tcp).
-behaviour(gen_server).
-author('cheepeero@gmx.net').
-author('humasect@gmail.com').

%% API
-export([start_link/0]).
-export([broadcast/1]).

%% callbacks
-export([init/1, terminate/2,
         handle_call/3, handle_cast/2, handle_info/2,
         code_change/3]).

%% internal
-export([acceptor_init/1]).

-define(SERVER, ?MODULE).
-define(LISTEN_OPTS, [binary, {packet,raw},
                      {reuseaddr,true}, {active,false}]).

-define(ACCEPT_OPTS, [binary, {packet,raw},
                      %% {keepalive,true}, {nodelay,true},
                      {active,once}]).


%% see inet:setopts/2 for valid options
%% if not set, the following values are used:
%% 	      {tcp_opts, [{keepalive, true},
%% 			  {nodelay, true},
%% 			  {tos, 16#14}]},  % MINDELAY and RELIABILITY
%% see man page ip(7) -> netinet/ip.h for TOS values

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    Port = zen_app:get_env(tcp_port),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

broadcast(Msg) ->
    gen_server:cast(?SERVER, {broadcast, Msg}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-define(spawn_acceptor, spawn_link(?MODULE, acceptor_init, [ListenSocket])).

init([Port]) ->
    case gen_tcp:listen(Port, ?LISTEN_OPTS) of
        {ok,ListenSocket} ->
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
    erlang:monitor(process, Pid),
    unlink(Acceptor),
    {noreply, {ListenSocket, ?spawn_acceptor, [Pid | Clients]}}
        ;
handle_cast({broadcast, Msg}, State = {_, _, Clients}) ->
    io:format("~w clients.~n", [length(Clients)]),
    lists:foreach(fun(C) -> C ! {send,Msg} end, Clients),
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid2, Reason}, {LS,Acceptor,Clients}) ->
    NewClients = lists:delete(Pid2, Clients),
    %%io:format("client died: ~p~n", [Reason]),
    {noreply, {LS, Acceptor, NewClients}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

acceptor_init(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok,Socket} ->
            {ok,{Address,_Port}} = inet:peername(Socket), % same in zen_client
            io:format("~w connected.~n", [Address]),
            gen_server:cast(?SERVER, {add_client, self()}),
            inet:setopts(Socket, ?ACCEPT_OPTS),
            zen_client:loop({tcp_client, Socket, waiting_auth});
        Else ->
            io:format("*** accept returned ~w.", [Else]),
            exit({error, {bad_accept, Else}})
    end.
