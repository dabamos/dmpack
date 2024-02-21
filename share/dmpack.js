/* Additional formatting of ISO 8601 time stamps in the DMPACK web interface. */
function formatTimeElements()
{
    var times = document.getElementsByTagName('time');

    for (var i = 0; i < times.length; i++)
    {
        var attr = times[i].getAttribute('datetime');
        if (attr.length != 25) continue;
        var date = attr.substring(0, 10);
        var time = attr.substring(11, 19);
        var zone = attr.substring(19);
        times[i].innerHTML = `${date} ${time} ${zone}`;
    }
}

formatTimeElements();
