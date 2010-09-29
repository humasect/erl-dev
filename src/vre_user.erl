%%%-------------------------------------------------------------------
%%% @author Lyndon Tremblay <humasect@gmail.com>
%%% @copyright (C) 2010, Lyndon Tremblay
%%% @doc
%%%
%%% @end
%%% Created : 24 Sep 2010 by Lyndon Tremblay <humasect@gmail.com>
%%%-------------------------------------------------------------------
-module(vre_user).
-author('humasect@gmail.com').
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(user, {login_id}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Id) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Id], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Id]) ->
    {ok, #user{login_id = Id}}.

handle_call([{<<"client">>, <<"get_time">>}], _From, State) ->
    {_Mega,Sec,_Micro} = now(),
    {reply, {send, [{result, [{ok, Sec}]}]}, State}
        ;
handle_call({logged_in, ActorId, Group, Name}, _From, State) ->
    Result = [{result,
               [{ok, [atom_to_binary(Group, latin1),
                      [{user, [ActorId,
                               list_to_binary(Name)]}]]}]}],
    {reply, {send, Result}, State}
        ;
handle_call([{<<"exit">>, _Reason}], _From, State) ->
    {stop, normal, ok, State}
        ;
handle_call(_Request, _From, State) ->
    {stop, unknown_message, {error,unknown_message}, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
