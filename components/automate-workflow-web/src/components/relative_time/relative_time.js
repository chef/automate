import ng from 'angular';
import moment from 'moment';

relativeTimeComponent.$inject = ['$interval'];

function relativeTimeComponent($interval) {

  function link(scope, element, attrs) {

    let interval;
    let delay = 1000 * 60;
    let count = 1;

    function renderRelativeTime(datetime) {
      if (interval) {
        $interval.cancel(interval);
      }

      element.text(moment.utc(datetime).fromNow());

      interval = $interval(() => renderRelativeTime(datetime), delay, count);
    }

    attrs.$observe('datetime', renderRelativeTime);

    scope.$on('$destroy', () => {
      if (interval) {
        $interval.cancel(interval);
      }
    });
  }

  return {
    link: link
  };
}

export default ng
  .module('cd.components.relativeTime', [])
  .directive('cdRelativeTime', relativeTimeComponent)
  .name;
