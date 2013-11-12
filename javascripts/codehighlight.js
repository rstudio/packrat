setTimeout(function() {
  var preBlocks = document.getElementsByTagName("pre");
  for (var i = 0; i < preBlocks.length; i++) {
    var codeBlocks = preBlocks[i].getElementsByTagName("code");
    for (var j = 0; j < codeBlocks.length; j++) {
      var html = codeBlocks[j].innerHTML;
      html = html.replace(/^((&gt;|\$) [^\r\n]*)$/gm, '<span class="command">$1</span>');
      codeBlocks[j].innerHTML = html;
    }
  }
}, 1);
