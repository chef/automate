import { initialState } from './event-feed.reducer';
import moment from 'moment';

describe('eventFeedReducer', () => {
  it('ensure the end date is at the end of the local time zone\'s day', () => {
    expect(initialState.filters.endDate.valueOf()).toEqual(moment().endOf('day').valueOf());
  });

  it('ensure the start date is at the beginning of the local time zone\'s day', () => {
    expect(initialState.filters.startDate.valueOf()).toEqual(moment().subtract(6, 'days').startOf('day').valueOf());
  });
});
