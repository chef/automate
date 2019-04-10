import ng from 'angular';
import pipeStatusListTemplate from './pipe_status_list.html';

function pipeStatusListDirective() {

  function link(scope) {

  }

  return {
    restrict: 'A',
    template: pipeStatusListTemplate,
    link: link,
    scope: {
      pipeStatus: '='
    }
  };
}

export default ng
  .module('cd.components.dashboard.pipeStatusList', [])
  .directive('cdPipeStatusList', pipeStatusListDirective)
  .name;
