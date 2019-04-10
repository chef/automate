import { TimeDurationPipe } from './time-duration.pipe';

describe('TimeDurationPipe', () => {
  let pipe;

  beforeEach(() => {
    pipe = new TimeDurationPipe();
  });

  it('converts a duration to a human-readable string', () => {
    expect(pipe.transform(24, 'hours')).toEqual('a day');
    expect(pipe.transform(27, 'days')).toEqual('a month');
    expect(pipe.transform(365, 'seconds')).toEqual('6 minutes');
    expect(pipe.transform(365, 'days')).toEqual('a year');
    expect(pipe.transform(86400, 'seconds')).toEqual('a day');
  });

});
