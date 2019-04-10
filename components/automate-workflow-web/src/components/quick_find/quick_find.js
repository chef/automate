import ng from 'angular';
import bsCompiler from 'angular-strap/dist/modules/compiler';
import bsParseOptions from 'angular-strap/dist/modules/parse-options';
import bsDimensions from 'angular-strap/dist/modules/dimensions';
import bsTooltip from 'angular-strap/dist/modules/tooltip';
import bsTooltipTpl from 'angular-strap/dist/modules/tooltip.tpl';
import bsTypeahead from 'angular-strap/dist/modules/typeahead';
import bsTypeaheadTpl from 'angular-strap/dist/modules/typeahead.tpl';
import quickFindController from './quick_find_controller';
import quickFindTemplate from './quick_find.html';

quickFindComponent.$inject = ['$state'];

function quickFindComponent($state) {
  function link(scope, element, attrs, ctrl) {
    scope.$on('$typeahead.select', (event, value) => {
      ctrl.selectResult(value);
    });
  }

  return {
    controller: 'quickFindController',
    controllerAs: 'quickFindCtrl',
    link: link,
    template: quickFindTemplate
  };
}

export default ng
  .module('cd.components.quickFind', [
    'mgcrea.ngStrap.helpers.parseOptions',
    'mgcrea.ngStrap.tooltip',
    'mgcrea.ngStrap.typeahead',
    quickFindController
  ])
  .directive('cdQuickFind', quickFindComponent)
  .name;
