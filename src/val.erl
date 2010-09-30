%%%-------------------------------------------------------------------
%%% @author Lyndon Tremblay <humasect@gmail.com>
%%% @copyright (C) 2010, Lyndon Tremblay
%%% @doc
%%%
%%% @end
%%% Created : 29 Sep 2010 by Lyndon Tremblay <humasect@gmail.com>
%%%-------------------------------------------------------------------
-module(val).
-author('humasect@gmail.com').

%% API
-export([init_db/0]).

-include("valhalla.hrl").

%%%===================================================================
%%% Database
%%%===================================================================

init_db() ->
    ?init_table(val_room, ordered_set),
    ?init_table(val_actor, ordered_set).
