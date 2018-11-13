
var community = 5;

window.onload = function(){document.getElementById('comm_frame').src = "pages/graphs/community_5.html";};

function prevcomm() {
    if(community > 1) { community = community - 1; }
    else { community = 18; }

    document.getElementById('comm_frame').src = "pages/graphs/community_"+community+".html";
}

function nextcomm() {
  if(community <= 19) { community = community + 1; }
  else { community = 1; }
    document.getElementById('comm_frame').src = "pages/graphs/community_"+community+".html";
}
