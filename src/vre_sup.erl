%%%-------------------------------------------------------------------
%%% @author Lyndon Tremblay <humasect@gmail.com>
%%% @copyright (C) 2010, Lyndon Tremblay
%%% @doc
%%%
%%% @end
%%% Created : 22 Sep 2010 by Lyndon Tremblay <humasect@gmail.com>
%%%-------------------------------------------------------------------
-module(vre_sup).
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
    SupFlags = {one_for_one, 10, 3600},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    UserSup = {'AName', {'AModule', start_link, []},
              Restart, Shutdown, Type, ['AModule']},

    {ok, {SupFlags, [UserSup]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================