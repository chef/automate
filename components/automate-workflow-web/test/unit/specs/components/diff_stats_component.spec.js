import ng from 'angular';
import diffStatsComponent from '../../../../src/components/diff_stats/diff_stats';

describe('diffStatsComponent', () => {
  let scope, element;

  beforeEach(ng.mock.module(diffStatsComponent));

  function createDirective(elementString) {

    return inject(($compile) => {
      element = $compile(ng.element(elementString))(scope);
      scope.$digest();
    });
  }

  beforeEach(inject(($rootScope) => {
    scope = $rootScope.$new();
  }));

  describe('with valid values', () => {

    beforeEach(() => {
      createDirective('<div diff-stats insertions="120" deletions="68"></div>');
    });

    it('renders the values', () => {
      let el = element[0];
      expect(el.querySelector('.stat.insertions').textContent).toBe('120');
      expect(el.querySelector('.stat.deletions').textContent).toBe('68');
    });

    it('renders the graph', () => {
      let el = element[0];
      expect(el.querySelector('.graph .insertions').style.width).toMatch(/\d+%$/);
      expect(el.querySelector('.graph .deletions').style.width).toMatch(/\d+%$/);
    });
  });

  describe('with invalid values', () => {

    beforeEach(() => {
      createDirective('<div diff-stats insertions="null" deletions=""></div>');
    });

    it('renders nothing', () => {
      let el = element[0];
      expect(el.querySelector('.stat.insertions')).toBeNull();
      expect(el.querySelector('.stat.deletions')).toBeNull();
      expect(el.querySelector('.graph')).toBeNull();
    });
  });
});
