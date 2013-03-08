function done(preview, file)
{
    var prew = document.getElementById("preview");
    while (prew.hasChildNodes()){
        prew.removeChild(prew.lastChild);
    }
    if ( typeof preview == 'string')
    {
        var img = document.createElement("img");
        img.src = preview;
        prew.appendChild(img);
    }
    else
    {
        preview.forEach(function (element, index, array){
            var img = document.createElement("img");
            img.src = element;
            prew.appendChild(img);
        });
    }
    document.getElementById("pic").value = file;
}