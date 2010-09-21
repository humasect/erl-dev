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

-export([loop/1, send_object/2]).

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
            send_object(State, Object),
            ?MODULE:loop(State);
        Else ->
            closed(State, Else)
    after
        ?TCP_TIMEOUT ->
            closed(State, timeout)
    end.

close_game(_State) -> closed.
%%     val_game_sup:close_game(Id);

%%%===================================================================
%%% messaging
%%%===================================================================

unwrap_message([Msg])               -> unwrap_message(Msg);
unwrap_message({obj,Object})        -> unwrap_message(Object);
unwrap_message({Name, {obj,Props}}) -> {Name, Props};
unwrap_message({Name, Props})       -> {Name, Props};
unwrap_message(Msg)                 -> Msg.

%%wrap_message([Msg]) -> {obj, [{result, Msg}]};
%%wrap_message(Msg)   -> {obj, [{result, {obj, Msg}]}}.
wrap_message(Msg)   -> {obj, [{result, {obj, Msg}}]}.

send_result(State, Result) ->
    send_object(State, [{result, {obj, [Result]}}]).

process_data(Data, Client) ->
    {ok,Msg1,_Remain} = json:decode(Data),
    Msg = unwrap_message(Msg1),
    io:format("message = ~p~n", [Msg]),

    case Msg of
        {"client", RealMsg} ->
            handle_message(unwrap_message(RealMsg), Client);
        {Module, _} ->
            send_result(Client,
                        {error, [unknown_module, list_to_binary(Module)]})
    end.
    %FName = list_to_existing_atom("msg_"++Name),
    %erlang:apply(?MODULE, FName, [State|Args]).

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
                    [#account{id=Id} = Account] ->
                        Update = Account#account{last_time=erlang:localtime(),
                                                 last_ip=Ip},
                        mnesia:write(Update),
                        case IsLogged(Ip) of
                            true -> id_in_use;
                            false -> {ok, Id}
                        end;
                    _ -> bad_credentials
                end
        end,
    case mnesia:transaction(F) of
        {atomic,Result} -> Result;
        {aborted,Reason} -> {error,Reason}
    end.

-ifdef(euaeuaoeuaoeu).
authorize({Name, Password, Ip}) ->
    F = fun() ->
                case mnesia:match_object(#login{name=Name, _='_'}) of
                    [] -> no_such_user;
                    [#login{id=Id, password=Password} = Login] ->
                        Update = Login#login{last_time=erlang:localtime(),
                                             last_ip=Ip},
                        mnesia:write(Update),

                        case IsLogged(Id) of
                            true -> already_logged;
                            false -> {ok, Id}
                        end;
                    _ -> wrong_password
                end
        end,

    case mnesia:transaction(F) of
        {atomic,Result} -> Result;
        {aborted,Reason} -> {error, Reason}
    end.
-endif().

handle_message({"login", [Username, Password, Language]}, State) ->
    Ip = fun ({web, WS, _}) ->
                 WS:get(peer_addr);
             ({tcp, S, _}) ->
                 {Address,_Port} = inet:peername(S),
                 Address
         end,
    Auth = {binary_to_list(Username),
            binary_to_list(Password),
            Ip(State)},

    case ?MODULE:authorize(Auth) of
        {ok,_Id} ->
            io:format("log in: ~p~n", [Auth]),
            {ok, State};
        Else ->
            Lang = binary_to_existing_atom(Language,latin1),
            Text = zen_data:get_text(Lang, Else),
            send_result(State, {error, list_to_binary(Text)}),
            {error, Else, Auth}
    end
    ;
handle_message({"command", <<$/,Cmd/binary>>}, State) ->
    io:format("errrrrrrrrrrrr, ~p~n", [Cmd]),
    {ok, State}
        ;
handle_message(Msg, State = {_SockType, _Socket, _Status}) ->
    %%Game = val_game_sup:which_game(Id),
    %%gen_server:call(game, Msg),
    io:format("game message: ~p~n", [Msg]),
    {ok, State}.

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

send_object(Client, Object) ->
    Send = case is_list(Object) of
               true -> Object;
               false -> [Object]
           end,
    send_raw(Client, [json:encode({obj, Send}), $\n]).

-ifdef(aijosdfioje).
ip_address(websocket, WS) ->
    WS:get(peer_addr);
ip_address(socket, S) ->
    {Address,_Port} = inet:peername(S),
    Address.
-endif.
