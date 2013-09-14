var allNames;
var allComments;

function lispify(arr)
{
    var rez = "(";
    arr.forEach(function (el, ind, a) {
        rez += '"' + el + "\" ";
    });
    rez += ")";
    return rez;
}

function initArray(len, val) {
    var rez = new Array(len), i = 0;
    while (i < len) { rez[i++] = val;}
    return rez;
}

function changeName(val, id)
{
    var ttl = document.getElementById("title");
    allNames[id] = val;
    ttl.value = lispify(allNames);
}

function changeComment(val, id)
{
    var cmt = document.getElementById("comment");
    allComments[id] = val;
    cmt.value = lispify(allComments);
}

function makePicDialog(preview, id)
{
    var div = document.createElement("div");
    var img = document.createElement("img");
    img.src = preview;
    var nameBox = document.createElement("div");
    nameBox.className = "name-box";
    var nameL = document.createElement("div");
    nameL.className = "name-label";
    nameL.innerText = "Название";
    var name = document.createElement("input");
    name.type = "text";
    name.className = "name-input";
    name.name = "title";
    name.onkeyup = function(e) {changeName(e.target.value, id);};
    nameBox.appendChild(nameL);
    nameBox.appendChild(name);
    var descrBox = document.createElement("div");
    descrBox.className = "comment-box";
    var descrL = document.createElement("div");
    descrL.className = "comment-label";
    descrL.innerText = "Описание";
    var descr = document.createElement("input");
    descr.type = "text";
    descr.className = "comment-input";
    descr.name = "comment";
    descr.onkeyup = function(e) {changeComment(e.target.value, id);};
    descrBox.appendChild(descrL);
    descrBox.appendChild(descr);
    div.appendChild(nameBox);
    div.appendChild(descrBox);
    div.appendChild(img);
    div.style.maxHeight = "100%";
    div.style.maxWidth = "100%";
    return div;
}

function done(preview, file)
{
    var prew = document.getElementById("preview");
    while (prew.hasChildNodes()){
        prew.removeChild(prew.lastChild);
    }
    if ( typeof preview == 'string')
    {
        prev.appendChild(makePicDialog( preview, 0));
        allNames = initArray(1, "");
        allComments = initArray(1, "");
    }
    else
    {
        preview.forEach(function (element, index, array){
            prew.appendChild(makePicDialog( element, index));
        });
        allNames = initArray(preview.length, "");
        allComments = initArray(preview.length, "")
    }
    document.getElementById("pic").value = file;
    document.getElementById("file-upload-input").disabled = true;
    changeName("", 0);
    changeComment("", 0);
}