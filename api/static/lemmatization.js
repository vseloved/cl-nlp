function submit_lemma() {
    $.post("lemmatize", JSON.stringify({
        "raw": $("#input").val()
    }), function(data) {
        $("#output").html(data.result);
    });
    return false;
}
