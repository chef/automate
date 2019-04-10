import { initialState } from './event-feed.reducer';
import * as moment from 'moment';

describe('eventFeedReducer', () => {
  it('ensure the end date is at the end of the local time zone\'s day', () => {
    expect(initialState.filters.endDate).toEqual(moment().endOf('day'));
  });

  it('ensure the start date is at the beginning of the local time zone\'s day', () => {
    expect(initialState.filters.startDate).toEqual(moment().subtract(6, 'days').startOf('day'));
  });
});
