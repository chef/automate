import ng from 'angular';
import 'angular-mocks';
import capitalizeFilter from '../../../../../src/common/filters/capitalize';

describe('capitalizeFilter', () => {

  beforeEach(ng.mock.module(capitalizeFilter));

  it('should capitalize its input', inject(($filter) => {
    let input = 'foobar';
    let result = $filter('capitalize')(input);
    expect(result).toBe('Foobar');
  }));
});
