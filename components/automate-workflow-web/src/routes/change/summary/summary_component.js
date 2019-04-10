import ng from 'angular';
import summaryTemplate from './summary.html';

function summaryComponent() {
  return {
    template: summaryTemplate
  };
}

export default ng
  .module('cd.routes.change.summary.component', [])
  .directive('cdSummary', summaryComponent)
  .name;
