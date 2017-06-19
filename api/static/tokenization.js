function submit_tok() {
    $.post("tokenize", JSON.stringify({
        "op": $("#tok-type").val(),
        "raw": $("#input").val()
    }), function(data) {
        $("#output").html(data.html);
    });
    return false;
}
