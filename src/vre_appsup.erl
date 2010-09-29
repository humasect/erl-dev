%%%-------------------------------------------------------------------
%%% @author Lyndon Tremblay <humasect@gmail.com>
%%% @copyright (C) 2010, Lyndon Tremblay
%%% @doc
%%%
%%% @end
%%% Created : 28 Sep 2010 by Lyndon Tremblay <humasect@gmail.com>
%%%-------------------------------------------------------------------
-module(vre_appsup).
-author('humasect@gmail.com').

-behaviour(application).
-behaviour(supervisor).

%% API
-export([priv_dir/0, priv_dir/1, get_env/1]).

%% Application callbacks
-export([start/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

priv_dir() ->
    code:priv_dir(nsv).
priv_dir(Name) ->
    filename:join(?MODULE:priv_dir(), Name).

get_env(Key) ->
    {ok,Value} = application:get_env(nsv, Key),
    Value.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    SupFlags = {one_for_one, 1, 3600},

    BucketCtl = {nsv_bucketctl, {nsv_bucketctl, start_link, []},
                 permanent, 2000, worker, [nsv_bucketctl]},

    {ok, {SupFlags, [BucketCtl]}}.

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    case supervisor:start_link({local, ?SERVER}, ?MODULE, []) of
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
