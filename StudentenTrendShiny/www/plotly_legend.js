function ChangeLabels(gElement) {
  var plot       = $(".js-plotly-plot:visible")
     ,hoverLayer = plot.find(".hoverlayer")
     ,hoverText  = hoverLayer.find(".name");
     
  if(hoverText) {  
    if(hoverText.html() == 'black') {
      hoverText.html("Totaallijn")
    } else if (hoverText.html() == 'gray48') {
      hoverText.html("Totaallijn geselecteerden")
    }
    
    if(gElement.attr("TextChange") != "true") {
      gElement.on('DOMNodeInserted', function(e) {
        gElement.attr("TextChange", "true");
        ChangeLabels(gElement);
      });
    }
    
    if($(hoverText).attr("TextChange") != "true") {
      $(hoverText).bind("DOMSubtreeModified",function(){
        $(hoverText).attr("TextChange", "true");
        ChangeLabels(gElement);
      });
    }
  } 
}

$(document).on('DOMNodeInserted', function(e) {
  if ($(e.target).is('g')) {
    ChangeLabels($(e.target));
  }
});
