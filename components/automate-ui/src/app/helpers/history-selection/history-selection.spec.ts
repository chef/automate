import { HistorySelection } from './history-selection';
import * as moment from 'moment';

describe('HistorySelection', () => {
  it('contains the correct list of selections for the selector component', () => {
    expect(HistorySelection.ALL).toEqual([
      'Last 24 hours', 'Last week', 'Last month', 'Last 3 months'
    ]);
  });

  describe('subtractAndFormat', () => {
    let fakeNow: moment.Moment;

    beforeEach(() => {
      fakeNow = moment.utc('1776-07-04T11:22:33Z');
    });

    it('supports "Last 24 hours"', () => {
      const actual = HistorySelection.subtractAndFormat(fakeNow, 'Last 24 hours');
      expect(actual).toEqual('1776-07-03T11:22:33Z');
    });

    it('supports "Last week"', () => {
      const actual = HistorySelection.subtractAndFormat(fakeNow, 'Last week');
      expect(actual).toEqual('1776-06-27T11:22:33Z');
    });

    it('supports "Last month"', () => {
      const actual = HistorySelection.subtractAndFormat(fakeNow, 'Last month');
      expect(actual).toEqual('1776-06-04T11:22:33Z');
    });

    it('supports "Last 3 months"', () => {
      const actual = HistorySelection.subtractAndFormat(fakeNow, 'Last 3 months');
      expect(actual).toEqual('1776-04-04T11:22:33Z');
    });

    it('defaults to now when parameter is not recognized', () => {
      const actual = HistorySelection.subtractAndFormat(fakeNow, 'Last century');
      expect(actual).toEqual('1776-07-04T11:22:33Z');
    });
  });
});

