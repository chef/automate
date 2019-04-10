import ng from 'angular';

modalContentDirective.$inject = ['$compile'];

function modalContentDirective($compile) {

  function link(scope, element, attrs) {
    var content = $compile(attrs.template)(scope);
    element.append(content);
  }

  return {
    restrict: 'A',
    link: link
  };
}

export default ng
  .module('cd.common.ui.modal.content', [])
  .directive('cdModalContent', modalContentDirective)
  .name;
