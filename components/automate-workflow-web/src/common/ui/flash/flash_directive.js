import ng from 'angular';
import ngAnimate from 'angular-animate';
import FlashService from './flash_service';
import flashTemplate from './flash.html';

flashDirective.$inject = [
  '$compile',
  '$animate',
  '$interval',
  'Flash'
];

function flashDirective($compile, $animate, $interval, Flash) {
  var timeout = 5000;

  function showFlash(element, flash) {
    $animate.enter(flash, element).then(function () {
      var timer = $interval(function () {
        hideFlash(flash, timer);
      }, timeout, 1);
    });
  }

  function hideFlash(flash, timer) {
    if (timer) $interval.cancel(timer);
    $animate.leave(flash);
  }

  function link(scope, element, attrs) {
    Flash.register(function(header, message, type) {
      var flash;

      var flashScope = scope.$new(true);

      flashScope.header = header;
      flashScope.message = message;
      flashScope.type = type;

      flash = $compile(flashTemplate)(flashScope);
      showFlash(element, flash);

      flash.on('click', function (event) {
        var target = event.target;

        if (target.className.match(/close/)) {
          hideFlash(flash);
          scope.$apply();
        }
      });
    });
  }

  return {
    link: link,
    scope: {}
  };
}

export default ng
  .module('cd.common.ui.flash.directive', [
    ngAnimate,
    FlashService
  ])
  .directive('cdFlash', flashDirective)
  .name;
