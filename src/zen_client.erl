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
       %% {'DOWN', Ref, process, Pid2, Reason} ->
        %    closed(
        {tcp_closed,_Socket} ->
            closed(Client, tcp_closed)
    after
        ?TCP_TIMEOUT ->
            closed(Client, timeout)
    end.

%%%===================================================================
%%% messaging
%%%===================================================================

process_data(Data, Client) ->
    Msg = jsx:json_to_term(Data),
    io:format("message = ~p~n", [Msg]),
    case Msg of
        [{<<"client">>, [{<<"login">>, [User, Pass]}]}] ->
            login({User, Pass, vre_user, english}, Client);
        [{<<"login">>, [User, Pass, Language]}] ->
            login({User, Pass, val_game, Language}, Client);
        Else ->
            handle_message(Else, Client)
        %%Else ->
        %%    send(Client, [{error, <<"unknown_message">>}]),
        %%    {error, unknown_message, Else}
    end.

handle_message([{<<"command">>, <<$/,Cmd/binary>>}], Client) ->
    %% @todo only if user is 'dev', evaluate erlang.
    io:format("errrrrrrrrrrrr, ~p~n", [Cmd]),
    {ok, Client}
        ;
handle_message([{<<"client">>, [{<<"say">>, Text}]}],
               Client = {_SockType, _Socket, #in_game{id=Id}})
  when is_binary(Text) ->
    zen_tcp:broadcast([{person_said, [Id, Text]}]),
    {ok, Client}
        ;
handle_message(Msg, Client = {_SockType, _Socket, #in_game{game_pid=Game}}) ->
    %%Game = zen_session_sup:which_session(Id),
    case gen_server:call(Game, Msg) of
        ok ->
            {ok, Client};
        {send,Result} ->
            send(Client, Result),
            {ok, Client};
        {error,Reason} ->
            send(Client, [{error, atom_to_binary(Reason, latin1)}]),
            closed(Client, {error, Reason, Msg})
    end.

close_client({SockType, Socket, #in_game{id=Id, game_pid=Game}}) ->
    io:format("close game ~p~n", [Id]),
    zen_session_sup:stop_session(Id),
    %%erlang:demonitor(Game),
    unlink(Game),
    {SockType, Socket, closed}
        ;
close_client({SockType, Socket, _}) ->
    {SockType, Socket, closed}.

%%%===================================================================
%%% authentication
%%%===================================================================

login({Login, Password, Mod, _Lang},
      Client = {SockType, Socket, waiting_auth}) ->
    Auth = {binary_to_list(Login),
            binary_to_list(Password),
            ip_address(SockType, Socket),
            Mod},
    case ?MODULE:authorize(Auth) of
        {ok,Group,#account{id=Id,actor_id=ActorId,name=Name}} ->
            io:format("log in: ~p~n", [Auth]),
            Game = zen_session_sup:start_session(Id, Mod),
            link(Game),
            
            NewClient = {SockType, Socket, #in_game{id=Id,
                                                    module=Mod,
                                                    game_pid=Game}},
            handle_message({logged_in, ActorId, Group, Name}, NewClient);
        Else ->
            %%Lang = binary_to_existing_atom(Language,latin1),
            %%Text = zen_data:get_text(Lang, Else),
            send(Client, [{result, [{error, atom_to_binary(Else,latin1)}]}]),
            {error, Else, Auth}
    end.

is_playing(Id) ->
    case zen_session_sup:which_session(Id) of
        undefined -> false;
        _ -> true
    end.

%%authorize({"dev", "dev", {127,0,0,1}, _Mod}) ->
%%    {ok, dev, 0, "Developer"};
%%authorize({"dev", "dev", _Ip, _Mod}) ->
%%    {error,bad_credentials};
authorize({Login, Password, Ip, _Mod}) ->
    F = fun() ->
                case mnesia:match_object(#account{login=Login,
                                                  password=Password,
                                                  _='_'}) of
                    [#account{id=Id,auth_count=Count} = Account] ->
                        Update = Account#account{last_time=erlang:localtime(),
                                                 last_ip=Ip,
                                                 auth_count=Count+1},
                        mnesia:write(Update),
                        case is_playing(Id) of
                            true -> id_in_use;
                            false ->
                                Group = zen_web:user_group(Login),
                                {ok, Group, Account}
                        end;
                    _ -> bad_credentials
                end
        end,
    case mnesia:transaction(F) of
        {atomic,Result} -> Result;
        {aborted,Reason} -> {error,Reason}
    end.

%%%===================================================================
%%% socket
%%%===================================================================

closed(Client = #web_client{socket=WS}, Reason) ->
    io:format("web socket: ~w ~w ~p.~n",
              [ip_address(web_client,WS), WS:get(socket), Reason]),
    close_client(Client)
        ;
closed(Client = #tcp_client{socket=S}, Reason) ->     
    io:format("tcp socket: ~w ~w ~p.~n",
              [ip_address(tcp_client,S), S, Reason]),
    close_client(Client).

send_raw(#web_client{socket=WS}, Data) ->
    WS:send(Data)
        ;
send_raw(#tcp_client{socket=S}, Data) ->
    gen_tcp:send(S, Data).

send(Client, Object) ->
    send_raw(Client, [jsx:term_to_json(Object), $\n]).

ip_address(web_client, WS) ->
    WS:get(peer_addr);
ip_address(tcp_client, S) ->
    case inet:peername(S) of
        {ok,{Address,_Port}} -> Address;
        _Else -> {0,0,0,0}
    end.



