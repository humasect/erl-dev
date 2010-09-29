%%%-------------------------------------------------------------------
%%% @author Lyndon Tremblay <humasect@gmail.com>
%%% @copyright (C) 2010, Lyndon Tremblay
%%% @doc
%%%
%%% @end
%%% Created : 28 Sep 2010 by Lyndon Tremblay <humasect@gmail.com>
%%%-------------------------------------------------------------------
-module(vre).
-author('humasect@gmail.com').

%% API
-export([init_db/0]).

-include("vrenvironment.hrl").

%%%===================================================================
%%% Actors
%%%===================================================================

create_actor(LoginId, Def) ->
    
    ok.

%%%===================================================================
%%% Database
%%%===================================================================

init_db() ->
    ?init_table(vre_object, ordered_set).
