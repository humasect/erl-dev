%%%-------------------------------------------------------------------
%%% @author Lyndon Tremblay <humasect@gmail.com>
%%% @copyright (C) 2010, Lyndon Tremblay
%%% @doc
%%%
%%% @end
%%% Created :  9 Oct 2010 by Lyndon Tremblay <humasect@gmail.com>
%%%-------------------------------------------------------------------
-module(zen_game).
-author('humasect@gmail.com').
-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("huma.hrl").

-record(state, {login_id :: uinteger(),
                name,     %% will be record for each game name.
                path,
                port}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Id, Name) ->
    Registered = list_to_atom(
                   lists:flatten(io_lib:format("zen_game_~s_~p", [Name, Id]))),
    io:format("game ~s starting.~n", [Registered]),
    gen_server:start_link({local, Registered}, ?MODULE, [Id, Name], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Id, Name]) ->
    process_flag(trap_exit, true),
    Path = filename:join(code:priv_dir(Name), Name),
    Port = open_port({spawn, Path}, [{packet, 2}]),
    {ok, #state{login_id=Id,
                name=Name,
                path=Path,
                port=Port}} .

handle_call(Request, _From, State) ->
    State#state.port ! {self(), {command, Request}},
    receive
        {_Port, {data, Data}} ->
            {reply, Data, State}
    after
        5000 ->
            {stop, port_timeout, State}
    end .

handle_cast(_Msg, State) ->
    {noreply, State} .

handle_info({data, Data}, State) ->
    io:format("game data: '~s'~n", [Data]),
    {noreply, State} ;
handle_info(stop, State) ->
    {stop, shutdown, State} ;
handle_info({'EXIT', _Port, Reason}, State) ->
    {stop, {port_exited,Reason}, State} ;
handle_info(_Info, State) ->
    {noreply, State} .

terminate(_Reason, State) ->
    io:format("game terminated.~n"),
    port_close(State#state.port),
    ok .

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
