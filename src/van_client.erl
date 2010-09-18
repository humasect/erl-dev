%%%-------------------------------------------------------------------
%%% @author Lyndon Tremblay <humasect@gmail.com>
%%% @copyright (C) 2010, Lyndon Tremblay
%%% @doc
%%%
%%% @end
%%% Created : 18 Sep 2010 by Lyndon Tremblay <humasect@gmail.com>
%%%-------------------------------------------------------------------
-module(van_client).
-author('humasect@gmail.com').

-export([loop/1, send_object/2]).

-define(TCP_TIMEOUT, 60000).

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

close_game(State) -> closed.
%%     val_game_sup:close_game(Id);

%%%===================================================================
%%% messaging
%%%===================================================================

unwrap_message([Msg]) -> unwrap_message(Msg);
unwrap_message({obj,Object}) -> unwrap_message(Object);
unwrap_message({Name, {obj,Props}}) -> {Name, Props};
unwrap_message({Name, Props}) -> {Name, Props}.

process_data(Data, State) ->
    {ok,Msg1,_Remain} = json:decode(Data),
    Msg = unwrap_message(Msg1),
    io:format("message = ~p~n", [Msg]),
    handle_message(Msg, State).
    %FName = list_to_existing_atom("msg_"++Name),
    %erlang:apply(?MODULE, FName, [State|Args]).

handle_message({"command", <<$/,Cmd/binary>>}, State) ->
    io:format("errrrrrrrrrrrr, ~p~n", [Cmd]),
    {ok, State};

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
    close_game(State);
closed(State = {tcp, S, _Status}, Reason) ->     
    io:format("tcp socket: ~w ~p.~n", [S, Reason]),
    close_game(State).

send_raw({web, WS, _Status}, Data) ->
    WS:send(Data);
send_raw({tcp, S, _Status}, Data) ->
    gen_tcp:send(S, Data).

send_object(Client, Object) when is_list(Object) ->
    send_raw(Client, json:encode({obj, Object}));
send_object(Client, Object) ->
    send_raw(Client, json:encode({obj, [Object]})).

-ifdef(aijosdfioje).
ip_address(websocket, WS) ->
    WS:get(peer_addr);
ip_address(socket, S) ->
    {Address,_Port} = inet:peername(S),
    Address.
-endif.
