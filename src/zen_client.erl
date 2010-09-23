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

-define(TCP_TIMEOUT, 60000).
-export([authorize/1]).

-include("zen.hrl").

%%%===================================================================
%%% API
%%%===================================================================

loop(State = {tcp, Socket, _Status}) ->
    inet:setopts(Socket, [{active,once}]),
    receive
        {tcp,_,Data = <<${,_/binary>>} ->
            case process_data(Data, State) of
                {ok,NewState} ->
                    ?MODULE:loop(NewState);
                Else ->
                    closed(State, Else)
            end;
        {tcp,_,Data} ->
            closed(State, {invalid_data, Data});
        {send,Object} ->
            send(State, Object),
            ?MODULE:loop(State);
        Else ->
            closed(State, Else)
    after
        ?TCP_TIMEOUT ->
            closed(State, timeout)
    end.

close_game({SockType, Socket, {in_game, Id, _Game}}) ->
    %%val_game_sup:close_game(Id);
    {SockType, Socket, closed}
        ;
close_game({SockType, Socket, _}) ->
    {SockType, Socket, closed}.

%%%===================================================================
%%% messaging
%%%===================================================================

process_data(Data, Client) ->
    %%{ok,Msg1,_Remain} = jsonerl:decode(Data),
    %%Msg = unwrap_message(Msg1),
    Msg = jsonerl:decode(Data),
    io:format("message = ~p~n", [Msg]),
    case Msg of
        {{<<"client">>, RealMsg}} ->
            handle_message(RealMsg, Client);
        Else ->
            send(Client, {{error, unknown_message}}),
            {unknown_message, Else}
    end.

authorize({Login, Password, Ip}) ->
    IsLogged = fun(_Id) ->
%%% case val_game_sup:which_game(Id) of
%%%     undefined -> false;
%%%     _ -> true
%%% end.                       
                       false
               end,
    F = fun() ->
                case mnesia:match_object(#account{login=Login,
                                                  password=Password,
                                                  _='_'}) of
                    [#account{id=Id,name=Name} = Account] ->
                        Update = Account#account{last_time=erlang:localtime(),
                                                 last_ip=Ip},
                        mnesia:write(Update),
                        case IsLogged(Id) of
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

handle_message({{<<"login">>, [Login, Password]}},
               Client = {SockType, Socket, waiting_auth}) ->
    Auth = {binary_to_list(Login),
            binary_to_list(Password),
            ip_address(SockType, Socket)},
    case ?MODULE:authorize(Auth) of
        {ok,Group,Id,Name} ->
            io:format("log in: ~p~n", [Auth]),
            send(Client, {{result, {{ok, [Group, Id, Name]}}}}),
            {ok, {SockType, Socket, {in_game, Id, undefined}}};
        Else ->
            %%Lang = binary_to_existing_atom(Language,latin1),
            %%Text = zen_data:get_text(Lang, Else),
            send(Client, {{result, {{error, list_to_binary(Else)}}}}),
            {error, Else, Auth}
    end
        ;
handle_message({{<<"command">>, <<$/,Cmd/binary>>}}, Client) ->
    io:format("errrrrrrrrrrrr, ~p~n", [Cmd]),
    {ok, Client}
        ;
handle_message({{<<"say">>, Text}},
               Client = {_SockType, _Socket, {in_game,_Id,_Game}}) ->
    zen_tcp:broadcast({{person_said, Text}}),
    {ok, Client}
        ;
handle_message(Msg, Client = {_SockType, _Socket, {in_game,_Id,_Game}}) ->
    %%Game = val_game_sup:which_game(Id),
    %%gen_server:call(game, Msg),
    io:format("game message: ~p~n", [Msg]),
    {ok, Client}
        ;
handle_message(Msg, State) ->
    closed(State, {error, unhandled_message, Msg}).

%%%===================================================================
%%% socket
%%%===================================================================

closed(State = {web, WS, _Status}, Reason) ->
    io:format("web socket: ~w ~p.~n", [WS:get(socket), Reason]),
    close_game(State)
        ;
closed(State = {tcp, S, _Status}, Reason) ->     
    io:format("tcp socket: ~w ~p.~n", [S, Reason]),
    close_game(State).

send_raw({web, WS, _Status}, Data) ->
    WS:send(Data)
        ;
send_raw({tcp, S, _Status}, Data) ->
    gen_tcp:send(S, Data).

send(Client, Object) ->
    Send = case is_list(Object) of
               true -> Object;
               false -> [Object]
           end,
    send_raw(Client, [jsonerl:encode(Send), $\n]).

ip_address(web, WS) ->
    WS:get(peer_addr);
ip_address(tcp, S) ->
    {ok,{Address,_Port}} = inet:peername(S),
    Address.


