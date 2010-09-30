%%%-------------------------------------------------------------------
%%% @author Lyndon Tremblay <humasect@gmail.com>
%%% @copyright (C) 2010, Lyndon Tremblay
%%% @doc
%%%
%%% @end
%%% Created :  7 Aug 2010 by Lyndon Tremblay <humasect@gmail.com>
%%%-------------------------------------------------------------------

-include("huma.hrl").

%%%-type(class() :: {atom(), atom(), atom()}).

-record(class, {attribute, job, family}).

%%------------%%
%% Map Server %%
%%------------%%

-record(val_room,
        {
          %%triggers=[],
          cells=[]
        }).

-record(map,
        {
          id :: uinteger(),
          class,
          rooms=[] :: #val_room{}
        }).

-record(cell_index,
        {
          map_id :: uinteger(),
          room_id :: uinteger(),
          location={0,0} :: #point{}
         }).

%%--------------%%
%% Actor Server %%
%%--------------%%

-define(MAX_PLAYER_INVENTORY, 24).
-define(MAX_CHAR_INVENTORY, 1).
-define(MAX_STORAGE, 80).

-record(item_dict,
        {
          amount=1 :: pos_integer(),
          status=normal :: cursed | blessed | normal,
          cost=0 :: uinteger(),
          charge_cost=0 :: uinteger(),
          weight=1 :: uinteger()
        }).

-record(char_dict,
        {
          party_id=0 :: uinteger(),
          base_level={1,0}, % :: range(),
          job_level={1,0}, % :: range(),
          stats=[],
          inventory=[] :: [#item_dict{}],
          equipment=[] :: [#item_dict{}]
        }).

-record(val_actor,
        {
          id :: uinteger(),
          cell_index :: #cell_index{},

          class,
          dict :: [#item_dict{} | #char_dict{}]
        }).

%%-------------%%
%% Game Server %%
%%-------------%%

-record(world,
        {
          id :: uinteger(),
          seed = random:seed() :: seed()
        }).
