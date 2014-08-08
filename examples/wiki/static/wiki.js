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

var r1 = null;
var r2 = null;

function setr1(wp, me) {
    r1 = me;

    if(r1 != null && r2 != null) {
        showDiff(wp);
    }
}

function setr2(wp, me) {
    r2 = me;

    if(r1 != null && r2 != null) {
        showDiff(wp);
    }
}

function showDiff(wp) {
    var fdata = new FormData();
    fdata.append("wp", wp);
    fdata.append("r1", r1);
    fdata.append("r2", r2);

    var req = new XMLHttpRequest();
    req.onload = function() {
        document.getElementById("diff").innerHTML = this.responseText;
    };

    req.open("post", "/-/diff", true);
    req.send(fdata);
}
