
var community = 2;

window.onload = function(){document.getElementById('comm_frame').src = "pages/graphs/community_5.html";};

function prevcomm() {
    community = community - 1;
    document.getElementById('comm_frame').src = "pages/graphs/community_"+community+".html";
}

function nextcomm() {
    community = community + 1;

}
