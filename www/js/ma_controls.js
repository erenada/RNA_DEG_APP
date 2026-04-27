// MA Tab - Advanced Options Toggle

$(document).ready(function() {
  $('#show_ma_advanced').click(function() {
    $('#ma_advanced_panel').slideToggle();
    var icon = $(this).find('i');
    if (icon.hasClass('fa-cog')) {
      icon.removeClass('fa-cog').addClass('fa-times');
      $(this).html('<i class="fa fa-times"></i> Hide Advanced Options');
    } else {
      icon.removeClass('fa-times').addClass('fa-cog');
      $(this).html('<i class="fa fa-cog"></i> Advanced Options');
    }
  });
});
