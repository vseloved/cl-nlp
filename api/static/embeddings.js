function submit_embed() {
    $.post("embed", JSON.stringify({
        "raw": $("#input").val(),
        "vecs": $("#vecs").val()
    }), function(data) {
        $("#output").html(data.html);
    });
    return false;
}
