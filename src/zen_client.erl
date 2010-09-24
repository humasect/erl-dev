%%%-------------------------------------------------------------------
%%% @author Lyndon Tremblay <humasect@gmail.com>
%%% @copyright (C) 2010, Lyndon Tremblay
%%% @doc
%%%
%%% @end
%%% Created : 18 Sep 2010 by Lyndon Tremblay <humasect@gmail.com>
%%%-------------------------------------------------------------------
-module(zen_client).
-author('humasect@gmail.com').

-export([loop/1]).

-define(TCP_TIMEOUT, 120*1000).
-export([authorize/1]).

-include("zen_account.hrl").

-record(in_game,
        {id :: uinteger(),
         module :: atom(),
         game_pid :: pid()}).

-define(client_def,
        {socket,
         status :: waiting_auth | #in_game{}}).

-record(tcp_client, ?client_def).
-record(web_client, ?client_def).

%%%===================================================================
%%% API
%%%===================================================================

loop(Client = #tcp_client{socket=Socket}) ->
    inet:setopts(Socket, [{active,once}]),
    receive
        {tcp,_,Data = <<${,_/binary>>} ->
            case process_data(Data, Client) of
                {ok,NewState} ->
                    ?MODULE:loop(NewState);
                Else ->
                    closed(Client, Else)
            end;
        {tcp,_,Data} ->
            closed(Client, {invalid_data, Data});
        {send,Object} ->
            send(Client, Object),
            ?MODULE:loop(Client);
        Else ->
            closed(Client, Else)
    after
        ?TCP_TIMEOUT ->
            closed(Client, timeout)
    end.

close_client({SockType, Socket, #in_game{id=Id, module=Mod}}) ->
    Mod:client_stop(Id),
    {SockType, Socket, closed}
        ;
close_client({SockType, Socket, _}) ->
    {SockType, Socket, closed}.

%%%===================================================================
%%% messaging
%%%===================================================================

process_data(Data, Client) ->
    Msg = jsx:json_to_term(Data),
    io:format("message = ~p~n", [Msg]),
    case Msg of
        [{<<"client">>, RealMsg}] ->
            handle_message(RealMsg, Client);
        Else ->
            send(Client, [{error, <<"unknown_message">>}]),
            {error, unknown_message, Else}
    end.

is_playing(Id) ->
    case zen_session_sup:which_session(Id) of
        undefined -> false;
        _ -> true
    end.

authorize({Login, Password, Ip, _Mod}) ->
    F = fun() ->
                case mnesia:match_object(#account{login=Login,
                                                  password=Password,
                                                  _='_'}) of
                    [#account{id=Id,name=Name,auth_count=Count} = Account] ->
                        Update = Account#account{last_time=erlang:localtime(),
                                                 last_ip=Ip,
                                                 auth_count=Count+1},
                        mnesia:write(Update),
                        case is_playing(Id) of
                            true -> id_in_use;
                            false ->
                                Group = zen_web:user_group(Login),
                                {ok, Group, Id, Name}
                        end;
                    _ -> bad_credentials
                end
        end,
    case mnesia:transaction(F) of
        {atomic,Result} -> Result;
        {aborted,Reason} -> {error,Reason}
    end.

handle_message([{<<"login">>, Creds}],
               Client = {SockType, Socket, waiting_auth}) ->
    {Mod,Login,Password,_Language} =
        case Creds of
            [User,Pass] -> {vre_game,User,Pass,english};
            [User,Pass,Lang] -> {val_game,User,Pass,Lang}
        end,
    Auth = {binary_to_list(Login),
            binary_to_list(Password),
            ip_address(SockType, Socket),
            Mod},
    case ?MODULE:authorize(Auth) of
        {ok,Group,Id,Name} ->
            io:format("log in: ~p~n", [Auth]),
            Game = zen_session_sup:start_session(Id, Mod),
            send(Client, [{result,
                           [{ok,
                             [atom_to_binary(Group, latin1),
                              Id,
                              list_to_binary(Name)]}]}]),
            {ok, {SockType, Socket, #in_game{id=Id,
                                             module=Mod,
                                             game_pid=Game}}};
        Else ->
            %%Lang = binary_to_existing_atom(Language,latin1),
            %%Text = zen_data:get_text(Lang, Else),
            send(Client, [{result, [{error, Else}]}]),
            {error, Else, Auth}
    end
        ;
handle_message([{<<"command">>, <<$/,Cmd/binary>>}], Client) ->
    io:format("errrrrrrrrrrrr, ~p~n", [Cmd]),
    {ok, Client}
        ;
handle_message([{<<"say">>, Text}],
               Client = {_SockType, _Socket, #in_game{}}) ->
    zen_tcp:broadcast([{person_said, Text}]),
    {ok, Client}
        ;
handle_message(Msg, Client = {_SockType, _Socket, #in_game{game_pid=Game}}) ->
    io:format("game message: ~p~n", [Msg]),
    %%Game = zen_session_sup:which_session(Id),
    gen_server:call(Game, Msg),
    {ok, Client}
        ;
handle_message(Msg, State) ->
    closed(State, {error, unhandled_message, Msg}).

%%%===================================================================
%%% socket
%%%===================================================================

closed(Client = #web_client{socket=WS}, Reason) ->
    io:format("web socket: ~w ~p.~n", [WS:get(socket), Reason]),
    close_client(Client)
        ;
closed(Client = #tcp_client{socket=S}, Reason) ->     
    io:format("tcp socket: ~w ~p.~n", [S, Reason]),
    close_client(Client).

send_raw(#web_client{socket=WS}, Data) ->
    WS:send(Data)
        ;
send_raw(#tcp_client{socket=S}, Data) ->
    gen_tcp:send(S, Data).

send(Client, Object) ->
    io:format("aoeuaoeu ~p~n", [Object]),
    send_raw(Client, [jsx:term_to_json(Object), $\n]).

ip_address(web_client, WS) ->
    WS:get(peer_addr);
ip_address(tcp_client, S) ->
    {ok,{Address,_Port}} = inet:peername(S),
    Address.


