import * as moment from 'moment-timezone';

import { DatetimePipe } from './datetime.pipe';

describe('DatetimePipe', () => {
  let pipe;

  beforeEach(() => {
    pipe = new DatetimePipe();
  });

  xit('returns a formatted string when given a datetime string', () => {
    const datetimeString = '2018-04-09T14:02:45.000Z';
    const formatStr = 'YYYY, MMMM D';
    const result = pipe.transform(datetimeString, formatStr);
    expect(result).toEqual('2018, April 9');
  });

  xit('returns a formatted string when given a moment datetime', () => {
    const datetime = moment('2018-04-09T14:02:45.000Z');
    const formatStr = 'YYYY, MMMM D';
    const result = pipe.transform(datetime, formatStr);
    expect(result).toEqual('2018, April 9');
  });

  xit('returns a formatted string when given a Date', () => {
    const datetime = new Date(2018, 3, 9);
    const formatStr = 'YYYY, MMMM D';
    const result = pipe.transform(datetime, formatStr);
    expect(result).toEqual('2018, April 9');
  });

});
