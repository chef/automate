import ng from 'angular';
import 'angular-mocks';
import relativeTimeComponent
  from '../../../../src/components/relative_time/relative_time';
import moment from 'moment';

describe('relativeTimeComponent', () => {
  let scope, element;

  beforeEach(ng.mock.module(relativeTimeComponent));

  function createDirective() {
    return inject(($compile, $rootScope) => {
      scope = $rootScope.$new();

      scope.datetime = '1970-01-01 00:00:00';
      element = $compile(ng.element(
        `<time
          datetime="{{ datetime }}"
          cd-relative-time>{{ datetime }}
        </time>`
      ))(scope);

      scope.$digest();
    });
  }

  describe('display text', () => {
    let relativeTime;

    beforeEach(() => {
      spyOn(moment, 'utc').and.returnValue({
        fromNow() { return relativeTime; }
      });
    });

    it('displays a relative time version of the provided datetime', () => {
      relativeTime = '3 hours ago'
      createDirective();
      expect(element.text()).toEqual(relativeTime);
    });

    it('updates every minute', inject(($interval) => {
      relativeTime = 'just now';
      createDirective();
      expect(element.text()).toEqual(relativeTime);

      relativeTime = 'one minute ago';
      $interval.flush(1000 * 60);
      expect(element.text()).toEqual(relativeTime);
    }));
  });

  describe('cleanup', () => {

    it('cancels the relative time interval', inject(($interval) => {
      spyOn($interval, 'cancel');

      createDirective();
      scope.$broadcast('$destroy');

      expect($interval.cancel).toHaveBeenCalled();
    }));
  });
});
