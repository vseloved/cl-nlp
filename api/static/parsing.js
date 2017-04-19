function submit_trees() {
    $.post("pprint", JSON.stringify({"raw": $("#input").val()}),
           function(data) {
               $("#output").html(data.html);
           });
    return false;
}
