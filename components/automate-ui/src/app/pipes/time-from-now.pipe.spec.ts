import * as moment from 'moment';

import { TimeFromNowPipe } from './time-from-now.pipe';

describe('TimeFromNow', () => {
  let pipe;

  beforeEach(() => {
    pipe = new TimeFromNowPipe();
  });

  it('returns `-` when time is before 2000-01-01T00:00:00.000Z', () => {
    const result = pipe.transform('1999-01-01T00:00:00.000Z');
    expect(result).toEqual('-');
  });

  it('returns `moment.fromNow` when time is after 2000-01-01T00:00:00.000Z', () => {
    const time = '2018-01-01T00:00:00.000Z';
    const result = pipe.transform(time);
    expect(result).toEqual(moment(time).fromNow());
  });

  it('accepts a moment object', () => {
    const time = moment('2018-01-01T00:00:00.000Z');
    const result = pipe.transform(time);
    expect(result).toEqual(time.fromNow());
  });

});
