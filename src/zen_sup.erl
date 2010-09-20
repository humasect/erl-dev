%%%-------------------------------------------------------------------
%%% @author Lyndon Tremblay <humasect@gmail.com>
%%% @copyright (C) 2010, Lyndon Tremblay
%%% @doc
%%%
%%% @end
%%% Created : 17 Sep 2010 by Lyndon Tremblay <humasect@gmail.com>
%%%-------------------------------------------------------------------
-module(zen_sup).
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
     [
      %% zen_logger

      {zen_tcp, {zen_tcp, start_link, []},
       permanent, 2000, worker, [zen_tcp]},

      {zen_data, {zen_data, start_link, []},
       permanent, 2000, worker, [zen_data]},

      {zen_irc_sup, {zen_irc_sup, start_link, []},
       permanent, infinity, supervisor, [zen_irc_sup]}
     ]}
    }.

%%%===================================================================
%%% Internal functions
%%%===================================================================
