%%%-------------------------------------------------------------------
%%% @author Lyndon Tremblay <humasect@gmail.com>
%%% @copyright (C) 2010, Lyndon Tremblay
%%% @doc
%%%
%%% @end
%%% Created : 19 Sep 2010 by Lyndon Tremblay <humasect@gmail.com>
%%%-------------------------------------------------------------------
-module(zen_web).
-author('humasect@gmail.com').

%% API
-export([start/0, restart/0, stop/0]).
-export([add_user/4, delete_user/1]).

-include("zen.hrl").
-include("mod_auth.hrl").

-define(AUTH_PASS, "valhalla").

-define(webconf(X), ?MODULE:config(X)).
-define(webpidconf, ?webconf({pid_at, "game"})).
-export([config/1]).

%%%===================================================================
%%% API
%%%===================================================================

start() -> 
    inets:start(httpd, ?webconf(inets)).

restart() ->
    httpd:reload_config(?webconf(inets), non_disturbing).

stop() ->
    Pid = proplists:get_value(httpd, inets:services()),
    inets:stop(httpd, Pid).

add_user(Id, Login, Password, Group) ->
    Config = ?webpidconf,
    case mod_auth:get_user(Id, Config) of
        {ok,_User} -> {error,user_exists};
        _Else ->
            true = mod_auth:add_user(Login, [{password, Password},
                                             {userData, Id}
                                             | Config]),
            true = mod_auth:add_group_member(Group, Login, Config),
            ok
    end.

delete_user(Login) ->
    Config = ?webpidconf,
    case mod_auth:get_user(Login, Config) of
        {ok,_User} ->
            true = mod_auth:delete_user(Login, Config),
            lists:map(
              fun(G) ->
                      true = mod_auth:delete_group_member(G, Login, Config)
              end, list_user_groups(Login));
        Error -> Error
    end.

list_user_groups(Login) ->
    Groups = mod_auth:list_groups(?webpidconf),
    error.

-ifdef(hhihihihi).
authorize({Name, Password, _Ip}) ->
    %%IsLogged = fun(_Id) ->
    %% case val_game_sup:which_game(Id) of
    %%     undefined -> false;
    %%     _ -> true
    %% end.                       
    %%                   false
    %%           end,
    case mod_auth:get_user(Name, ?webpidconf) of
        {ok, User = #httpd_user{user_data = Id}}
          when User#httpd_user.password =:= Password ->
            {ok,Id};
        Error -> Error
    end.
-endif.

%%%===================================================================
%%% Internal functions　内部の関数
%%%===================================================================

config({auth_dir,Name}) ->
    {Realm,Groups} =
        case Name of
            "game" -> {"Gamelike", [dev, admin, player]};
            "edoc" -> {"Zen Edoc", [dev, admin]};
            "org" -> {"Zen Org", [dev, admin]};
            "stats" -> {"Gamelike Stats", [dev, admin, vendor]};
            _ -> {"Valhalla", [dev]}
        end,
    {?webconf(root) ++ "/" ++ Name ++ "/",
     [{auth_name,Realm},
      {auth_type,mnesia},
      {auth_access_password, ?AUTH_PASS},
      {require_group, Groups}]}
        ;
config(root) ->
    code:priv_dir(zen) ++ "/www"
        ;
config(port) ->
    {ok,Port} = application:get_env(zen, web_port),
    Port
        ;
config(inets) ->
    {ok,Hostname} = inet:gethostname(),
    [{server_root, code:priv_dir(zen) ++ "/log"},
     {document_root, ?webconf(root)},
     {server_name, Hostname},
     {port, ?webconf(port)},
     {bind_address, any},

     {error_log, "error.log"},
     {security_log, "security.log"},
     {transfer_log, "transfer.log"},

     {max_keep_alive_requests, 4},

     %%{security_directory, {Priv++"/www/game",
     %%                      [{security_data_file, "security.dat"}]}},

     {directory_index, ["index.html"]},
     {server_admin, "humasect@gmail.com"},

     {directory, ?webconf({auth_dir, "game"})},
     {directory, ?webconf({auth_dir, "stats"})},
     {directory, ?webconf({auth_dir, "edoc"})},
     {directory, ?webconf({auth_dir, "org"})},

     {modules, [zen_mod_websocket,
                mod_alias,
                %%mod_security,
                mod_auth,
                mod_esi, mod_actions, mod_cgi,
                mod_dir, mod_get, mod_head, mod_log, mod_disk_log,
                mod_htaccess, mod_include]}]
        ;
config({pid_at,DirName}) ->
    [{dir, ?webconf(root) ++ "/"++DirName++"/"},
     {port, ?webconf(port)},
     {authPassword,?AUTH_PASS}
    ].
