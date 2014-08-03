function changeToEditTab() {
    document.getElementById("edit").style.display    = "block";
    document.getElementById("preview").style.display = "none";

    document.getElementById("editLnk").style.fontWeight    = "bold";
    document.getElementById("previewLnk").style.fontWeight = "normal";
}

function changeToPreviewTab() {
    var fdata = new FormData();
    fdata.append("markup", document.getElementsByName("markup")[0].value);

    var req = new XMLHttpRequest();
    req.onload = function () {
        document.getElementById("edit").style.display    = "none";
        document.getElementById("preview").style.display = "block";

        document.getElementById("editLnk").style.fontWeight    = "normal";
        document.getElementById("previewLnk").style.fontWeight = "bold";

        document.getElementById("preview").innerHTML     = this.responseText;
    };

    req.open("post", "/-/preview", true);
    req.send(fdata);
}
