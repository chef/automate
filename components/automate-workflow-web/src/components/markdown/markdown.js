import ng from 'angular';
import ngSanitize from 'angular-sanitize';
import ngEmoji from 'angular-emoji-filter/lib/emoji';
import highlight from 'highlight.js';
import marked from 'marked';

markdownComponent.$inject = ['$sanitize', '$filter'];

marked.setOptions({
  highlight: function (code) {
    return highlight.highlightAuto(code).value;
  }
});

function markdownComponent($sanitize, $filter) {

  function link(scope, element, attrs) {
    if (attrs.cdMarkdown) {
      scope.$watch(attrs.cdMarkdown, render);
    } else {
      render(element.text());
    }

    function render(content) {
      var html = content ? $sanitize(marked($filter('emoji')(content))) : '';
      element.html(html);
    }
  }

  return {
    link: link
  };
}

export default ng
  .module('cd.components.markdown', [
    ngSanitize,
    'emoji'
  ])
  .directive('cdMarkdown', markdownComponent)
  .name;
