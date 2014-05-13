// function for making a panel group
function makePanelGroup(id) {
  var panelGroup = "<div class=\"panel-group\" id=\"" + id + "\"></div>";
  return panelGroup;
}

// function for making a panel for a cotegory
function makePanel(id, heading, style) {
  var panel =
    "<div class=\"panel panel-" + style + "\" id=\"panel-" + id + "\">" +
      "<div class=\"panel-heading\">" +
        "<h2 class=\"panel-title\">" +
          "<a data-toggle=\"collapse\" data-target=\"#collapse-" + id + "\" href=\"#collapse-" + id + "\"" +
          "onclick=\"$('#collapse-" + id + "').toggle();\">" +
          heading +
        "</a>" +
      "</h2>" +
    "</div>" +
    "<div id=\"collapse-" + id + "\" class=\"panel-collapse collapse\">" +
      "<div id=\"body-" + id + "\" class=\"panel-body\">" +
        "</div>" +
      "</div>" +
    "</div>";
  return panel;
}