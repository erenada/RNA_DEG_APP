// Experimental Design Tab - Drag and Drop Functionality
// Handles sample dragging between available samples and group zones

// Wait for both jQuery UI and Shiny to be ready
$(document).ready(function initWhenReady() {
  // Ensure jQuery UI is loaded
  if (typeof $.ui === 'undefined') {
    setTimeout(initWhenReady, 100);
    return;
  }
  
  // Make samples draggable
  function makeDraggable() {
    $('.sample-item:not(.ui-draggable)').draggable({
      revert: 'invalid',
      helper: 'clone',
      cursor: 'move',
      zIndex: 1000
    });
  }
  
  // Make drop zones droppable  
  function makeDroppable() {
    $('#group1_samples, #group2_samples, #available_samples').droppable({
      accept: '.sample-item',
      tolerance: 'pointer',
      over: function(event, ui) {
        $(this).addClass('dragover');
      },
      out: function(event, ui) {
        $(this).removeClass('dragover');
      },
      drop: function(event, ui) {
        $(this).removeClass('dragover');
        var sampleId = ui.draggable.attr('data-sample-id') || ui.draggable.data('sample-id');
        var targetZone = $(this).attr('id');
        
        // Send to Shiny
        Shiny.setInputValue('sample_moved', {
          sample: sampleId,
          target: targetZone,
          timestamp: Date.now()
        });
      }
    });
  }
  
  // Initialize drag and drop
  function initializeDragDrop() {
    makeDraggable();
    makeDroppable();
  }
  
  // Initial setup
  initializeDragDrop();
  
  // Re-initialize when specific outputs update
  // Use MutationObserver for robust detection of DOM changes
  var observer = new MutationObserver(function(mutations) {
    var shouldReinit = false;
    mutations.forEach(function(mutation) {
      if (mutation.addedNodes && mutation.addedNodes.length > 0) {
        shouldReinit = true;
      }
    });
    if (shouldReinit) {
      makeDraggable();
    }
  });
  
  // Start observing the containers
  var config = { childList: true, subtree: true };
  var availableList = document.getElementById('available_samples');
  var group1List = document.getElementById('group1_samples');
  var group2List = document.getElementById('group2_samples');
  
  if (availableList) observer.observe(availableList, config);
  if (group1List) observer.observe(group1List, config);
  if (group2List) observer.observe(group2List, config);
  
  // Also keep shiny:value as a fallback, but use a longer timeout and read attr
  $(document).on('shiny:value', function(event) {
    if (event.target.id === 'available_samples_list' || 
        event.target.id === 'group1_samples_list' || 
        event.target.id === 'group2_samples_list') {
      setTimeout(makeDraggable, 500);
    }
  });
});

