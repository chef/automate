import ng from 'angular';
import textExpanderTemplate from './text_expander.html';

function textExpanderComponent() {
  
  function link(scope, element, attrs) {

    element.addClass('expandable');
    
    var msgHeader = ng.element(element[0].querySelector('.msg-header'));
    msgHeader.on('click', function () {
      element.toggleClass('expanded');
    });

    scope.$on('$destroy', function () {
      msgHeader.off();
    });
  }

  return {
    link: link,
    template: textExpanderTemplate,
    scope: {
      patchset: '=cdTextExpander'
    }
  };
}

export default ng
  .module('cd.components.textExpander', [])
  .directive('cdTextExpander', textExpanderComponent)
  .name;
