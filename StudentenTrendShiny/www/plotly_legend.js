function RemoveLegendDashed() {
  var plot     = $(".js-plotly-plot:visible")
     ,legend   = plot.find(".groups");

  var elements = $(legend.find("path")).filter(function() {
      return $(this).css("stroke-dasharray") === '9px, 9px';
  });
  
  elements.parent().parent().css("display", "none");
  
  var legendtoggles = $('.legendtoggle');
  
  legendtoggles.removeAttr('pointer-events');
  legendtoggles.css('cursor', 'auto');
}

$(document).on('DOMNodeInserted', function(e) {
  if ($(e.target).is('path')) {
    RemoveLegendDashed();
  }
});