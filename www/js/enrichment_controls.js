// Enrichment Tab - Advanced Options Toggle and Gene Download Handler

$(document).ready(function() {
  // Advanced options toggle
  $('#show_advanced_options').on('click', function() {
    $('#advanced_options_panel').slideToggle();
    var icon = $(this).find('i');
    if (icon.hasClass('fa-cog')) {
      icon.removeClass('fa-cog').addClass('fa-times');
      $(this).html('<i class="fa fa-times"></i> Hide Advanced Options');
    } else {
      icon.removeClass('fa-times').addClass('fa-cog');
      $(this).html('<i class="fa fa-cog"></i> Advanced Options');
    }
  });
  
  // Handle gene download button clicks
  $(document).on('click', '.download-genes-btn', function() {
    var row = $(this).data('row');
    var ont = $(this).data('ont');
    var contrast = $(this).data('contrast');
    Shiny.setInputValue('download_gene_list', {
      row: row,
      ont: ont,
      contrast: contrast,
      timestamp: new Date().getTime()
    }, {priority: 'event'});
  });
});

