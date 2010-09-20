%%%-------------------------------------------------------------------
%%% @author Lyndon Tremblay <humasect@gmail.com>
%%% @copyright (C) 2010, Lyndon Tremblay
%%% @doc
%%%
%%% @end
%%% Created : 27 Aug 2010 by Lyndon Tremblay <humasect@gmail.com>
%%%-------------------------------------------------------------------
-module(zen).
-author('humasect@gmail.com').

%% API
-export([start_all/0, stop_all/0, init_db/0]).

-include("zen.hrl").
-include("mod_auth.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start_all() ->
    mnesia:start(),
    inets:start(),
    application:load(zen),
    zen_web:start(),
    application:start(zen).

stop_all() ->
    application:stop(zen),
    zen_web:stop(),
    inets:stop(),
    mnesia:stop().

-define(init_table(X,Y),
        huma_db:init_table(X, Y, record_info(fields, X))).
-define(init_table(X,Y,T),
        huma_db:init_table(X, Y, record_info(fields, X), T)).

init_db() ->
    %% ?init_table(login, ordered_set,
    %%             fun
    %%                 ({login, Id, Name, Password, Language,
    %%                   CreateTime, LastTime, LastIp}) ->
    %%                     #login{id=Id, name=Name,
    %%                            language=Language,
    %%                            create_time=CreateTime,
    %%                            last_time=LastTime,
    %%                            last_ip=LastIp}
    %%             end),

    ?init_table(httpd_user, bag),
    ?init_table(httpd_group, bag),

    %% ウーザを追加
    %% make sure we have some users.
    ?MODULE:add_user("dev", "dev", dev),
    ?MODULE:add_user("humasect", "sect0huma", user),
    ok.

-ifdef(asdfasd).
    ?init_table(actor, ordered_set),
    ?init_table(map  , ordered_set),
    ?init_table(world, ordered_set,
                fun
                    ({world,Id,Seed}) ->
                        #world{id=Id, seed=Seed}
                end),
    ok.
-endif.


%%%===================================================================
%%% Internal functions　内部の関数
%%%===================================================================

-ifdef(nothing_spectacular).

make_user_t(Name, Password) ->
    case mnesia:match_object(#login{name=Name, _='_'}) of
        [] -> ok;
        _ -> mnesia:abort(user_already_exists)
    end,

    Id = huma_db:next_id(login),
    make_world_t(Id),
    ActorId = make_actor_t(Id, {0,0}, {player, undef, undef, undef}),
    mnesia:write(#login{
                    id=Id,
                    name=Name,
                    password=Password,
                    actor_id=ActorId
                   }),
    Id.

make_world_t(Id) ->
    case mnesia:read({world, Id}) of
        [] ->
            mnesia:write(#world{
                            id = Id
                           }),
            {ok,Id};
        _Else -> already_exists
    end.

make_actor_t(WorldId, Location, Class) ->
    Id = huma_db:next_id(actor),
    mnesia:write(#actor{
                    id = Id,
                    room_id = WorldId,
                    location = Location,
                    class = Class
                   }),
    Id.

add_user(Name, Password) -> add_user(Name, Password, ?USER_ID).
add_user(Name, Password, StartId) ->
    mnesia:transaction(
      fun() ->
              make_user_t(Name, Password, StartId)
      end).

add_actor(Id) ->
    {error, not_implemented}.

delete_world_t(Id) ->
    mnesia:delete({world, Id}).

delete_user_t(Id) ->
    [L|_] = mnesia:read({login, Id}),
    mnesia:delete({actor, L#login.actor_id}),
    mnesia:delete({login, Id}).

remove_user(Id) ->
    mnesia:transaction(
      fun() ->
              delete_world_t(Id),
              delete_user_t(Id)
      end).
-endif.

