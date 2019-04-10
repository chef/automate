import ng from 'angular';
import ngScroll from 'angular-scroll';
import ansi_up from 'ansi_up';
import Session from '../../common/auth/session';
import Flash from '../../common/ui/flash/flash';
import logTemplate from './log.html';
import 'angular-feature-flags/dist/featureFlags';

logComponent.$inject = [
  '$document',
  '$window',
  '$http',
  '$httpParamSerializer',
  'Session',
  'featureFlags'
];

function logComponent($document, $window, $http, $httpParamSerializer, Session, featureFlags) {

  function link(scope, element, attrs) {
    scope.isOn = featureFlags.isOn;
    // TODO: Once we are sending the compliance test reults to the db, we should
    // be able to clean up some of these scope variables. Atm, we use viewPlugin
    // to detect if there are compliance test results in the log, viewCompliance
    // is used to control the switchview button, and changeView is set to the
    // opposite of current view to allow for personalized log toolbar options.
    scope.viewPlugin = 'log';
    scope.viewCompliance = false;
    scope.changeView = 'Compliance';
    scope.log = '';

    $http.get(scope.href).then((resp) => {
      scope.log = resp.data.run_log;
    });

    scope.$on('phase_run_updated', (e, data) => {
      if (data.href === scope.href) {
        scope.log = data.run_log;
        scope.$apply();
      }
    });

    scope.$watch('log', (log = '') => {
      scope.result = log.match("Compliance data has posted.");

      if (scope.result) {
        $http.get(scope.href + '/log_objects').then((resp) => {
          let logObjects = resp.data;

          if (logObjects.length > 0) {
            scope.viewPlugin = 'compliance';
            scope.viewPlugin = 'compliance';
            scope.viewCompliance = true;
            scope.changeView = 'Log';

            scope.json = JSON.parse(logObjects[0].data);
          }
        })
        .catch((resp) => {
          Flash.error('Error', resp.data.message);
        });
      }
    });

    element.bind('click', (e) => {
      e.stopPropagation();
    });

    let toolbar = angular.element(element[0].querySelector('.log-toolbar'));

    let headerOffset = 100;
    let duration = 0;

    scope.switchView = function () {
      scope.viewCompliance = !scope.viewCompliance;
      if (scope.changeView === 'Compliance') {
        scope.changeView = 'Log';
      }
      else {
        scope.changeView = 'Compliance';
      }
    };

    scope.scrollToTop = function () {
      $document.scrollTo(element[0], headerOffset, duration);
      scope.scrollable = false;
    };

    scope.scrollToBottom = function () {
      let offset = $window.innerHeight - element[0].offsetHeight;
      $document.scrollTo(element[0], offset, duration);
      scope.scrollable = true;
    };

    scope.handleScroll = function () {
      let rect = element[0].getBoundingClientRect();
      if (rect.top <= headerOffset) {
        toolbar.addClass('anchored');
      } else {
        toolbar.removeClass('anchored');
      }
    };

    scope.handleMouseWheel = function () {
      scope.scrollable = false;
    };

    attrs.$observe('toolbar', function (value) {
      if (value === 'false') {
        toolbar.remove();
      }
    });

    let downloadPath = `${scope.href}/log`;
    let { username, token } = Session.get();
    let sessionParams = {
      'chef-delivery-user': username,
      'chef-delivery-token': token
    };

    scope.$watch('colorize', (colorize = false) => {
      let downloadParams = Object.assign(sessionParams, { colorize: colorize });
      downloadParams = $httpParamSerializer(downloadParams);
      scope.downloadHref = `${downloadPath}?${downloadParams}`;
    });

    $window.addEventListener('scroll', scope.handleScroll);
    $window.addEventListener('mousewheel', scope.handleMouseWheel);

    scope.$on('$destroy', function () {
      $window.removeEventListener('scroll', scope.handleScroll);
      $window.removeEventListener('mousewheel', scope.handleMouseWheel);
    });

    scope.close = ($event) => {
      $event.stopPropagation();
      scope.scrollable = true;
      scope.$emit('closing_phase_log', scope.href);
    };

    scope.scrollable = true;

  }

  return {
    link: link,
    scope: {
      href: '=logHref'
    },
    template: logTemplate
  };
}

export default ng
  .module('cd.components.log', [
    'duScroll',
    Session,
    'feature-flags'
  ])
  .directive('cdLog', logComponent)
  .name;
