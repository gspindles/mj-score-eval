// function for making a panel
function makePanel(id, heading) {
  var panel =
    "<div class=\"panel panel-default\" id=\"panel-" + id + "\">" +
      "<div class=\"panel-heading\">" +
        "<h2 class=\"panel-title\">" +
          "<a data-toggle=\"collapse\" data-target=\"#collapse-" + id + "\" href=\"#collapse-" + id + "\"" +
          "onclick=\"$('#collapse-" + id + "').toggle();\">" +
          heading +
        "</a>" +
      "</h2>" +
    "</div>" +
    "<div id=\"collapse-" + id + "\" class=\"panel-collapse collapse\">" +
      "<div class=\"panel-body\">" +
        "</div>" +
      "</div>" +
    "</div>";
  return panel;
}