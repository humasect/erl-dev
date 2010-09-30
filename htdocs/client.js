//////////////////////////////////////////////////////////////////////
//
//////////////////////////////////////////////////////////////////////

client = 
{
    create_socket: function(User, Pass)
    {
        var Host = window.location.hostname?
            window.location.hostname: "localhost";
        var Socket = new WebSocket("ws://"+Host+":1980/");// +
                                   //"user="+User+"&pass="+Pass+"&lang="+
                                   //text.language());

        Socket.onopen = function(e)
        {
            //var Msg = {'login': [user, pass, get_language()]};
            //_client.socket.send(JSON.stringify(Msg));

            client.is_connected = true;
            $('#login_panel').hide();
            $('#game').show();
            $('#connect_status').html(text.get('connect'));
        };

        Socket.onclose = function(e)
        {
            if (!client.is_connected)
                alert("Could not connect to server!");

            client.is_connected = false;
            $('#login_panel').show();
            $('#game').hide();
            $('#connect_status').html(text.get('disconnect'));
        };

        Socket.onerror = function (e)
        {
            alert("socket error:" + e.type);
        };

        Socket.onmessage = function (e)
        {
            var Object = null;

            try {
                Object = JSON.parse(e.data);
            } catch (x) {
                log(e.data);
                log("Could not parse JSON!");
            }

            if (!Object)
                return;

            log(Object);

            for (var Key in Object)
            {
                var Found = null;

                for (var I in client.message_handlers)
                {
                    var Test = client.message_handlers[I][Key];
                    if (Test != undefined)
                    {
                        Found = Test;
                        break;
                    }
                }

                if (Found == null)
                {
                    log("Unknown message '" + key + "'.");
                    continue;
                }

                Found(client, object[key]);
            }
        };

        return Socket;
    },

//////////////////////////////////////////////////////////////////////
//
//////////////////////////////////////////////////////////////////////

    messages:
    {
        'error': function(Client, Message)
        {
            $('#login_status').html(Message);
            $('#login_panel').show();

            // disconnect..
        },

        'test_response': function(Client, Message)
        {
            log("OK.");
        }
    },

    start: function(User, Pass)
    {
        $('#connect_status').html(text.get('connecting'));

        client.is_connected = false;
        client.socket = client.create_socket(User, Pass);
        client.message_handlers = [client.messages, game.messages];
        localStorage['user_name'] = User;
    },

    send_object: function(Object)
    {
        var Message = JSON.stringify(Object);
        client.socket.send(Message);
    },
};

