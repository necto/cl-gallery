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
            img.style.maxHeight = "100%";
            img.style.maxWidth = "100%";
            prew.appendChild(img);
        });
    }
    document.getElementById("pic").value = file;
}