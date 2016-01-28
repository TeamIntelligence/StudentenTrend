var RemoveClickableLegend = function() {
  var plot           = $(".js-plotly-plot:visible")
      ,infoLayer     = plot.find(".infolayer")
      ,legend        = plot.find(".legend")
      ,legendGroups  = legend.find(".groups")
      ,legendTitle   = plot.find(".annotation-text-g")
      ,legendItems   = legendGroups.find(".traces")
      ,legendHeight  = parseInt(legend.attr("height")) * 0.5
      ,legendtoggles = plot.find('.legendtoggle')
      ,marginTop     = 0;
  
  if(legendItems.length > 3){
    var legendY      = parseInt(legend.attr("y"))
       ,legendTitleY = legendTitle.attr("y") === undefined ? 1 : parseInt(legendTitle.attr("y"));
       
    if(!isNaN(legendY) && !isNaN(legendTitleY)) {
      legend.attr("y", legendY + legendHeight);
      legendTitle.find("svg").attr("y", legendTitleY + legendHeight);
    }   
  }
  
  legendtoggles.removeAttr('pointer-events');
  legendtoggles.css('cursor', 'auto');
};

function ChangeLabels(gElement) {
  var plot       = $(".js-plotly-plot:visible")
     ,hoverLayer = plot.find(".hoverlayer")
     ,hoverText  = hoverLayer.find(".name");
     
  if(hoverText) {  
    if(hoverText.html() == 'black') {
      hoverText.html("Totaallijn");
    } else if (hoverText.html() == 'gray48') {
      hoverText.html("Totaallijn geselecteerden");
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

var tester = function(e) {
  if ($(e.target).is("g.groups")) {
    RemoveClickableLegend();
  }
};

$(document).on('DOMNodeInserted', function(e) {
  _.debounce(tester, 100)(e);
  
  if ($(e.target).is('g')) {
    ChangeLabels($(e.target));
  }
});
