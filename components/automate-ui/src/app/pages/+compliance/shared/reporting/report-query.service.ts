import { Injectable } from '@angular/core';
import { BehaviorSubject } from 'rxjs';
import * as moment from 'moment';
import { FilterC } from '../../+reporting/types';

export interface ReportQuery {
  startDate: moment.Moment;
  endDate: moment.Moment;
  interval: number;
  filters: FilterC[];
}

interface TimeIntervals {
  name: string;
  findStartDate: (endDate: moment.Moment) => moment.Moment;
}

@Injectable()
export class ReportQueryService {

  intervals: TimeIntervals[] = [
    { name: 'Last 10 days', findStartDate: (endDate: moment.Moment): moment.Moment =>
      moment(endDate).subtract(10, 'days')},
    { name: 'Last month', findStartDate: (endDate: moment.Moment): moment.Moment =>
      moment(endDate).subtract(1, 'months')},
    { name: 'Last 3 months', findStartDate: (endDate: moment.Moment): moment.Moment =>
      moment(endDate).subtract(3, 'months')},
    { name: 'Last year', findStartDate: (endDate: moment.Moment): moment.Moment =>
      moment(endDate).subtract(1, 'years')}
  ];

  private idToTitle: Map<string, string> = new Map<string, string>();

  state: BehaviorSubject<ReportQuery> = new BehaviorSubject(this.initialReportQueryState());

  private initialReportQueryState() {
    const endDate = moment().utc().startOf('day').add(12, 'hours');
    return {
      startDate: this.findTimeIntervalStartDate(0, endDate),
      endDate: endDate,
      interval: 0,
      filters: []
    };
  }

  getReportQuery(): ReportQuery {
    return this.state.getValue();
  }

  setState(newState: ReportQuery) {
    this.state.next(newState);
  }

  findTimeIntervalStartDate(interval: number, endDate: moment.Moment): moment.Moment {
    return this.intervals[interval].findStartDate(endDate);
  }

  setFilterTitle(type: string, id: string, title: string) {
    this.idToTitle.set(type + '-' + id, title);
  }

  getFilterTitle(type: string, id: string): string {
    return this.idToTitle.get(type + '-' + id);
  }
}
