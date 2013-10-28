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

function changeName(id, val)
{
    var ttl = document.getElementById("title");
    allNames[id] = val;
    ttl.value = lispify(allNames);
}

function changeComment(id, val)
{
    var cmt = document.getElementById("comment");
    allComments[id] = val;
    cmt.value = lispify(allComments);
}

function changeTime(id, val)
{
    var t = document.getElementById("time");
    if (null != t) //receive album page doesn't contain the time field
    {
        allTimes[id] = val;
        t.value = lispify(allTimes.map(function (time) { return time.toJSON();}));
    }
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
    name.onkeyup = function(e) {changeName(id, e.target.value);};
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
    descr.onkeyup = function(e) {changeComment(id, e.target.value);};
    descrBox.appendChild(descrL);
    descrBox.appendChild(descr);
    div.appendChild(nameBox);
    div.appendChild(descrBox);
    div.appendChild(img);
    div.style.maxHeight = "100%";
    div.style.maxWidth = "100%";
    return div;
}

function done(files)
{
    var prew = document.getElementById("preview");
    while (prew.hasChildNodes()){
        prew.removeChild(prew.lastChild);
    }
    var filePaths = files.map(function (el) { return el.file; });
    allTimes = initArray(preview.length, new Date());
    files.forEach(function (element, index, array){
        prew.appendChild(makePicDialog( element.url, index));
        allTimes[index] = new Date(element.date);
    });
    allNames = initArray(files.length, "");
    allComments = initArray(files.length, "");

    document.getElementById("pic").value = lispify(filePaths);
    document.getElementById("file-upload-input").disabled = true;
    changeName(0, "");
    changeComment(0, "");
    changeTime(0, allTimes[0]);
}
