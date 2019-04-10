import ng from 'angular';
import 'angular-mocks';
import momentFilter from '../../../../../src/common/filters/moment';

describe('momentFilter', () => {

  beforeEach(ng.mock.module(momentFilter));

  it('should format the provided date with the provided format', inject(($filter) => {
    let date = '2015-01-01', format, result;

    format = 'MMMM D, YYYY';
    result = $filter('moment')(date, format);
    expect(result).toBe('January 1, 2015');

    format = 'MM/DD/YYYY';
    result = $filter('moment')(date, format);
    expect(result).toBe('01/01/2015');
  }));
});
