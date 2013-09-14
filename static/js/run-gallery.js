$(document).ready(function() {
	$(".fancybox-thumb").fancybox({
		helpers	: {
			title	: {
				type: 'outside'
			},
			thumbs	: {
				width	: 50,
				height	: 50
			}
		}
	});
});

function reMasonry()
{
    $('#masContainer').masonry({
        columnWidth:110,
        itemSelector:'.img'
    });
}

window.onload = function() {
    reMasonry();
};

function editItem(id, event)
{
    var ttl, cmt;
    event.target.parentElement.hidden = true;
    document.getElementById("edit-" + id).hidden=false;
    reMasonry();
}

function AJAXGet(formId, url){
    var form = document.getElementById( formId);
    var elem = form.elements;
    var params = "";
    //url = form.action;
    for(var i = 0; i < elem.length; i++){
        var amp = "";
        if (i != 0) amp = "&";
        if (elem[i].name != ""){
            if (elem[i].tagName == "SELECT"){
                params += amp + elem[i].name + "=" + 
                    encodeURIComponent(
                        elem[i].options[elem[i].selectedIndex]
                            .value);
            }else{
                params += amp + elem[i].name + "=" + encodeURIComponent(elem[i].value);
            }
        }
    } 
    if (window.XMLHttpRequest){// code for IE7+, Firefox, Chrome, Opera, Safari
        xmlhttp=new XMLHttpRequest();
    }else{// code for IE6, IE5
        xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
    }
    url += "?" + params;
    xmlhttp.open("GET",url,false);
    xmlhttp.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
    xmlhttp.send("GET",url, false);
    return xmlhttp.responseText;    
}

function updateItem(id, url)
{
    var response = AJAXGet("update-form-" + id, url);
    var imgDiv = document.getElementById("img-" + id);
    var replacement = document.createElement("div");
    replacement.innerHTML = response;
    imgDiv.innerHTML = replacement.children[0].innerHTML;
    reMasonry();
}