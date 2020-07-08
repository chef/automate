import { initialState } from './event-feed.reducer';
import * as moment from 'moment/moment';

describe('eventFeedReducer', () => {
  it('ensure the end date is at the end of the local time zone\'s day', () => {
    expect(initialState.filters.endDate).toEqual(moment.utc().endOf('day'));
  });

  it('ensure the start date is at the beginning of the local time zone\'s day', () => {
    expect(initialState.filters.startDate).toEqual(moment.utc().subtract(6, 'days').startOf('day'));
  });
});
