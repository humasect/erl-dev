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
-module(zen_acceptor).
-behaviour(gen_server).
-author('cheepeero@gmx.net').
-author('humasect@gmail.com').

%% API
-export([start_link/2]).
-export([ip_address/1]).

%% callbacks
-export([init/1, terminate/2,
         handle_call/3, handle_cast/2, handle_info/2,
         code_change/3]).

%% internal
-export([acceptor_init/2]).

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

-record(state,
        {type,
         listen_socket,
         acceptor,
         clients}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Type, Port) ->
    gen_server:start_link({local, Type}, ?MODULE, [Type, Port], []).

%%broadcast(Msg) ->
%%    gen_server:cast(?SERVER, {broadcast, Msg}).

ip_address(Socket) ->
    case inet:peername(Socket) of
        {ok,{Address,_Port}} -> Address;
        _Else -> {0,0,0,0}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-define(spawn_acceptor(Type),
        spawn_link(?MODULE, acceptor_init, [Type, ListenSocket])).

init([Type, Port]) ->
    case gen_tcp:listen(Port, ?LISTEN_OPTS) of
        {ok,ListenSocket} ->
            {ok, #state{type=Type,
                        listen_socket=ListenSocket,
                        acceptor=?spawn_acceptor(Type),
                        clients=[]}};
        Error ->
            exit(Error)
    end.

terminate(_Reason, #state{listen_socket=ListenSocket,
                          acceptor=Acceptor}) ->
    io:format("** acceptor terminated. Should kill all clients.~n"),
    exit(Acceptor, kill),
    gen_tcp:close(ListenSocket),
    ok.

handle_call(_Call, _From, State) ->
    {reply, ok, State}.

handle_cast({add_client, Pid},
            State = #state{type=Type,
                           listen_socket=ListenSocket,
                           acceptor=Acceptor,
                           clients=Clients}) ->
    erlang:monitor(process, Pid),
    unlink(Acceptor),
    {noreply, State#state{acceptor = ?spawn_acceptor(Type),
                          clients = [Pid | Clients]}}
        ;
handle_cast({broadcast, Msg}, State = #state{clients=Clients}) ->
    io:format("~w clients.~n", [length(Clients)]),
    lists:foreach(fun(C) -> C ! {send,Msg} end, Clients),
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid2, Reason},
            State = #state{clients=Clients}) ->
    NewClients = lists:delete(Pid2, Clients),
    %%io:format("client died: ~p~n", [Reason]),
    {noreply, State#state{clients = NewClients}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

acceptor_init(Type = zen_acceptor_tcp, ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok,Socket} ->
            Address = ip_address(Socket),
            io:format("TCP ~w connected.~n", [Address]),
            gen_server:cast(Type, {add_client, self()}),
            inet:setopts(Socket, ?ACCEPT_OPTS),
            zen_client:loop({tcp_client, Socket, waiting_auth});
        Else ->
            io:format("*** accept returned ~w.", [Else]),
            exit({error, {bad_accept, Else}})
    end
        ;
acceptor_init(Type = zen_acceptor_web, ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok,Socket} ->
            Address = ip_address(Socket),
            io:format("WEB ~w connected.~n", [Address]),
            gen_server:cast(Type, {add_client, self()}),
            case gen_tcp:recv(Socket, 0, 5000) of
                {ok,Data} ->
                    Header = binary_to_list(Data),
                    io:format("WEB ~p: some data: ~s~n", [Address,Data]);
                Else ->
                    io:format("WEB ~p: no data. ~p~n", [Address,Else])
            end;
        Else ->
            io:format("*** accept returned ~w.", [Else]),
            exit({error, {bad_accept, Else}})
    end.
                        
