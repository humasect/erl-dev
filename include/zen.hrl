%%%-------------------------------------------------------------------
%%% @author Lyndon Tremblay <humasect@gmail.com>
%%% @copyright (C) 2010, Lyndon Tremblay
%%% @doc
%%%
%%% @end
%%% Created : 19 Sep 2010 by Lyndon Tremblay <humasect@gmail.com>
%%%-------------------------------------------------------------------

-include("huma.hrl").

%%--------------%%
%% Login        %%
%%--------------%%

-type(language() :: english | japanese).

%%-define(DEV_ID, 100000).
%%-define(USER_ID, 200000).

-record(login,
        {
          %%id :: uinteger(),
          name :: string(),
          %%password :: string(),
          actor_id :: uinteger(),
          language=english :: language(),
          create_time=erlang:localtime(),
          last_time,
          last_ip
        }).
