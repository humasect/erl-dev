game = {};

game_messages =
{
    change_to_room: function(client, dict)
    {
        log("�������ѹ�: "+dict);
    },

    add_actor: function(client, dict)
    {
        log("�����������ɲ�:");
        log(dict);
    }
};
