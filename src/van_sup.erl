%%%-------------------------------------------------------------------
%%% @author Lyndon Tremblay <humasect@gmail.com>
%%% @copyright (C) 2010, Lyndon Tremblay
%%% @doc
%%%
%%% @end
%%% Created : 17 Sep 2010 by Lyndon Tremblay <humasect@gmail.com>
%%%-------------------------------------------------------------------
-module(van_sup).
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
      {van_tcp_sup, {van_tcp_sup, start_link, []},
       permanent, infinity, supervisor, [van_tcp_sup]},

      {van_client_sup, {van_client_sup, start_link, []},
       permanent, infinity, supervisor, [van_client_sup]}
     ]}
    }.

%%%===================================================================
%%% Internal functions
%%%===================================================================
