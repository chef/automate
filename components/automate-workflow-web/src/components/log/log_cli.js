import ng from 'angular';
import ngScroll from 'angular-scroll';
import ansi_up from 'ansi_up';
import Session from '../../common/auth/session';
import logCliTemplate from './log_cli.html';

logCliComponent.$inject = [
  '$http',
  '$window',
  '$document'
];

function logCliComponent($http, $window, $document) {
  function link(scope, element, attrs) {

    let display = angular.element(element[0].querySelector('.log-display'));

    scope.$watch('log', (log = '') => {
      let escapedLog = ansi_up.escape_for_html(log);
      let coloredLog = ansi_up.ansi_to_html(escapedLog);

      display.html(coloredLog);
      if (scope.scrollable) {
        scope.scrollToBottom();
      }
    });

    scope.scrollToBottom = function () {
      let offset = $window.innerHeight - element[0].offsetHeight;
      $document.scrollTo(element[0], offset);
    };
  }
  return {
    link: link,
    scope: {
      log: '=log',
      scrollable: '=scrollable'
    },
    template: logCliTemplate,
  };
}

export default ng
  .module('cd.components.logCli', [
    'duScroll',
    Session
  ])
  .directive('cdCliLog', logCliComponent)
  .name;
