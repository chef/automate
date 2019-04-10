// TODO eng-ex discuss if this should be turned into a module
import * as moment from 'moment';

type Moment = moment.Moment;
type Duration = moment.Duration;

export class HistorySelection {
  // List of all supported selections to be displayed in the selector component
  static Durations: { [key: string]: Duration } = {
    'Last 24 hours': moment.duration(1, 'day'),
    'Last week'    : moment.duration(1, 'week'),
    'Last month'   : moment.duration(1, 'month'),
    'Last 3 months': moment.duration(3, 'months')
  };

  static ALL: string[] = Object.keys(HistorySelection.Durations);

  static startingTimestamp(range: string): string {
    return HistorySelection.subtractAndFormat(moment.utc(), range);
  }

  static lastWeek(): string {
    return HistorySelection.startingTimestamp('Last week');
  }

  static lastDay(): string {
    return HistorySelection.startingTimestamp('Last 24 hours');
  }

  static findIncludedDurationTerm(d: Date): string {
    const date = new Date(d);
    for (const key of HistorySelection.ALL) {
      const startingTimestamp = HistorySelection.startingTimestamp(key);
      const selectionDate = new Date(startingTimestamp);
      if (selectionDate.getTime() - date.getTime() < 0) {
        return key;
      }
    }
    return HistorySelection.ALL[0];
  }

  // private, exposed for testing.
  static subtractAndFormat(now: Moment, range: string): string {
    return now.subtract(HistorySelection.Durations[range]).format();
  }

}
