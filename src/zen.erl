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
-export([create_account/4,
         remove_account/1,
         list_accounts/0]).

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

-spec create_account(string(), string(), string(), user_group()) ->
                            ok | {error, term()}.
%%% @doc add account.
create_account(Login, Password, Name, Group) ->
    %%make_world_t(Id),
    ActorId = 0, %make_actor_t(Id, {0,0}, {player, undef, undef, undef}),

    F = fun() ->
                case mnesia:match_object(#account{login=Login, _='_'}) of
                    [] -> ok;
                    _ -> mnesia:abort(id_already_exists)
                end,

                Id = huma_db:next_id(account),
                mnesia:write(#account{id=Id,
                                      login=Login, password=Password,
                                      name=Name,
                                      actor_id=ActorId
                                     }),
                Id
        end,

    case mnesia:transaction(F) of
        {atomic,Id} ->
            zen_web:add_user(Id, Login, Password, Group);
        Else -> Else
    end.

list_accounts() ->
    mnesia:transaction(
      fun() -> mnesia:match_object(#account{_='_'}) end).

remove_account(Id) ->
    %% @todo
    %% kick user out of game.
    mnesia:transaction(fun() -> mnesia:delete({account,Id}) end),
    zen_web:delete_user(Id).

-define(init_table(X,Y),
        huma_db:init_table(X, Y, record_info(fields, X))).
-define(init_table(X,Y,T),
        huma_db:init_table(X, Y, record_info(fields, X), T)).

init_db() ->
    ?init_table(httpd_user, bag),
    ?init_table(httpd_group, bag),
    ?init_table(account, ordered_set,
                fun
                    ({account, Id, Login, Password, Name, Language,
                      ActorId,
                      CreateTime, LastTime, LastIp, _Empty}) ->
                        #account{id=Id,
                                 login=Login, password=Password,
                                 name=Name,
                                 language=Language,
                                 actor_id=ActorId,
                                 create_time=CreateTime,
                                 last_time=LastTime,
                                 last_ip=LastIp}
                end),

    %% ウーザを追加
    %% make sure we have some accounts.
    ?MODULE:create_account("dev", "dev", "Developer", dev),
    ?MODULE:create_account("humasect", "sect0huma", "Lyndon Tremblay", player),
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

