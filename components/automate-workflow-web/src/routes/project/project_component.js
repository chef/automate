import ng from 'angular';
import bsParseOptions from 'angular-strap/dist/modules/parse-options';
import bsTooltip from 'angular-strap/dist/modules/tooltip';
import bsTooltipTpl from 'angular-strap/dist/modules/tooltip.tpl';
import bsSelect from 'angular-strap/dist/modules/select';
import bsSelectTpl from 'angular-strap/dist/modules/select.tpl';
import bsCompiler from 'angular-strap/dist/modules/compiler';
import bsDimensions from 'angular-strap/dist/modules/dimensions';

import projectTemplate from './project.html';

function projectComponent() {
  return {
    template: projectTemplate
  };
}

export default ng
  .module('cd.routes.project.component', [
    'mgcrea.ngStrap.helpers.parseOptions',
    'mgcrea.ngStrap.tooltip',
    'mgcrea.ngStrap.select',
  ])
  .directive('cdProject', projectComponent)
  .name;
