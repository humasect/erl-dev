//////////////////////////////////////////////////////////////////////
//
//////////////////////////////////////////////////////////////////////

function login()
{
    var User = dom.element('user_name');
    if (User.value.trim() == "")
        return;

    var Pass = dom.element('pass_word');
    if (Pass.value.trim() == "")
        return;

    client.start(User.value, Pass.value);
    Pass.value = "";
}

function logout()
{
    client.socket.close();
}

function send_command()
{
    var Command = dom.element('command');
    if (Command.value.trim() == "")
        return;

    client.send_object({command: Command.value});
    Command.value = "";
}

function log(String)
{
    console.log(String);
}

window.onload = function()
{
    text.init();
    dom.element('user_name').value = localStorage['user_name'];

    function show_login()
    {
        dom.element('login_status').innerHTML = text.get('log_in');
        dom.show('login_panel');
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

