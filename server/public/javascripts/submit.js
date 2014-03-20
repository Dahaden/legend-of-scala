function createChar() {
  var chname = document.getElementById("input").value;
  if(chname == ""){
    warning("You give me nothing? I give you nothing!");
    return;
  }

  var chtoken = rand(9);

  var toSend = JSON.stringify({ name:chname, token:chtoken });

  $.post("/adventurers", toSend)
    .fail(function(err) {
      warning("Sorry that name is taken");
    })
    .done(function(res) {
      $("#warning").hide(200);
      $("#submit").hide(200);
      $("#name").html($("#name").html() + " " + chname);
      $("#token").html($("#token").html() + " " + chtoken);
      $("#confirm").show(200);
    });
}

function warning(text) {
  $("#warning").html(text);
  $("#warning").show(200);

}

function rand(length,current){
 current = current ? current : '';
 return length ? rand( --length , "0123456789ABCDEFGHIJKLMNOPQRSTUVWXTZabcdefghiklmnopqrstuvwxyz".charAt( Math.floor( Math.random() * 60 ) ) + current ) : current;
}
