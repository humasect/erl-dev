%%%-------------------------------------------------------------------
%%% @author Lyndon Tremblay <>
%%% @copyright (C) 2010, Lyndon Tremblay
%%% @doc
%%%
%%% @end
%%% Created : 17 Sep 2010 by Lyndon Tremblay <>
%%%-------------------------------------------------------------------
-module(van_tcp_sup).
-author('humasect@gmail.com').
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    {ok, {{one_for_one, 1, 60},
          [{van_tcp, {van_tcp, start_link, []},
            permanent, 60, worker, [van_tcp]}]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
