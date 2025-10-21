// Analysis Log Custom Message Handler
// Appends log messages to the analysis log pre element

Shiny.addCustomMessageHandler('appendLog', function(message) {
  var pre = document.getElementById('analysis_log');
  if (!pre) return;
  var now = new Date().toISOString().replace('T',' ').slice(0,19);
  var line = '[' + now + '] ' + message.text + '\n';
  pre.textContent += line;
  pre.scrollTop = pre.scrollHeight;
});

