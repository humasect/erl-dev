%%%-------------------------------------------------------------------
%%% @author Lyndon Tremblay <humasect@gmail.com>
%%% @copyright (C) 2010, Lyndon Tremblay
%%% @doc
%%%
%%% @end
%%% Created : 17 Sep 2010 by Lyndon Tremblay <humasect@gmail.com>
%%%-------------------------------------------------------------------
-module(zen_app).
-author('humasect@gmail.com').
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    %% @todo start logger.
    zen:init_db(),

    case zen_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
                end.

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
