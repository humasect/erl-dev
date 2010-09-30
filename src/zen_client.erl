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

%% API
-export([run/2]).

-define(TCP_TIMEOUT, 120*1000).
-export([authorize/1, loop/1]).

-include("zen_account.hrl").

-record(in_game,
        {id :: uinteger(),
         module :: atom(),
         game_pid :: pid()}).

-record(client,
        {socket,
         address,
         status :: waiting_auth | #in_game{}}).

%%%===================================================================
%%% API
%%%===================================================================

run(Address, Socket) ->
    ?MODULE:loop(#client{address = Address,
                         socket = Socket,
                         status = waiting_auth}).

loop(Client = #client{socket=Socket}) ->
    inet:setopts(Socket, [{active,once}]),
    Process = fun(Data) ->
                      case process_data(Data, Client) of
                          {ok,NewState} -> ?MODULE:loop(NewState);
                          Else -> closed(Client, Else)
                      end
              end,
    receive
        {tcp,_,Data = <<${,_/binary>>} ->
            Process(Data);
        {tcp,_,Data = <<0,${,_/binary>>} ->
            Process(binary:part(Data, 1, byte_size(Data)-2));
        {tcp,_,Data} ->
            io:format("oaeuaoeu '~s'~n", [Data]),
            %%case {binary:first(Data), binary:last(Data)} of
            %%    {0,255} -> 
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
    io:format("daaaaaaaata = '~s'~n", [Data]),
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
               Client = #client{status=#in_game{id=Id}})
  when is_binary(Text) ->
    zen_tcp:broadcast([{person_said, [Id, Text]}]),
    {ok, Client}
        ;
handle_message(Msg, Client = #client{status=#in_game{game_pid=Game}}) ->
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
    end
        ;
handle_message(Msg, _Client) ->
    {error, not_logged_in, Msg}.

close_client(Client = #client{status=#in_game{id=Id, game_pid=Game}}) ->
    io:format("close game ~p~n", [Id]),
    zen_session_sup:stop_session(Id),
    %%erlang:demonitor(Game),
    unlink(Game),
    Client#client{status=closed}
        ;
close_client(Client) ->
    Client#client{status=closed}.

%%%===================================================================
%%% authentication
%%%===================================================================

login({Login, Password, Mod, _Lang},
      Client = #client{address=Addr, status=waiting_auth}) ->
    Auth = {binary_to_list(Login),
            binary_to_list(Password),
            Addr, Mod},
    case ?MODULE:authorize(Auth) of
        {ok,Group,#account{id=Id,actor_id=ActorId,name=Name}} ->
            io:format("log in: ~p~n", [Auth]),
            Game = zen_session_sup:start_session(Id, Mod),
            link(Game),
            
            NewClient = Client#client{status = #in_game{id = Id,
                                                        module = Mod,
                                                        game_pid = Game}},
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

closed(Client = #client{address=Addr, socket=S}, Reason) ->     
    io:format("tcp socket: ~w ~w ~p.~n", [Addr, S, Reason]),
    close_client(Client).

send_raw(#client{socket=S}, Data) ->
    gen_tcp:send(S, Data).

send(Client, Object) ->
    send_raw(Client, [jsx:term_to_json(Object), $\n]).


