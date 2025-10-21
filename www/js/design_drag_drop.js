// Experimental Design Tab - Drag and Drop Functionality
// Handles sample dragging between available samples and group zones

// Wait for both jQuery UI and Shiny to be ready
$(document).ready(function() {
  // Ensure jQuery UI is loaded
  if (typeof $.ui === 'undefined') {
    setTimeout(arguments.callee, 100);
    return;
  }
  
  // Make samples draggable
  function makeDraggable() {
    $('.sample-item').draggable({
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
        var sampleId = ui.draggable.data('sample-id');
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
  $(document).on('shiny:value', function(event) {
    if (event.target.id === 'available_samples_list' || 
        event.target.id === 'group1_samples_list' || 
        event.target.id === 'group2_samples_list') {
      setTimeout(initializeDragDrop, 100);
    }
  });
});

