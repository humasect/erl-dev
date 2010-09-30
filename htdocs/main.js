//////////////////////////////////////////////////////////////////////
//
//////////////////////////////////////////////////////////////////////

function login()
{
    var User = $('#user_name');
    if (User.val().trim() == "")
        return;

    var Pass = $('#pass_word');
    if (Pass.val().trim() == "")
        return;

    client.start(User.val(), Pass.val());
    Pass.val("");
}

function logout()
{
    client.socket.close();
}

function send_command()
{
    var Command = $('#command');
    if (Command.val().trim() == "")
        return;

    client.send_object({command: Command.val()});
    Command.val("");
}

function log(String)
{
    console.log(String);
}

window.onload = function()
{
    text.init();
    $('#user_name').val(localStorage['user_name']);

    function show_login()
    {
        $('#login_status').html(text.get('log_in'));
        $('#login_panel').show();
    }

    ('WebSocket' in window)
        ? show_login()
        : alert(text.get('no_websocket'));

    //
    // デバッグで行く!
    //

    return;

    if (0)
        start_client("dev", "dev");
    else
    {
        _client = {
            is_connected: true
        };
        dom.show('game');
        dom.hide('login_panel');
    }
};

document.onkeydown = function(e)
{
    if (!client.is_connected)
        return;

    var angle_map = {
        "d": 270,
        "b": 180,
        "f": 90,
        "h": 0,

        "g": 315,
        "i": 45,
        "c": 135,
        "a": 225
    };

    var s = String.fromCharCode(e.which || e.keyCode);

    if (s in angle_map)
        send_message({'move': angle_map[s]});
		//world.turn ({target: world.player, angle: angle});
	else
		console.log("テスト '" + s + "'");
};

document.onkeyup = function(event)
{
};

