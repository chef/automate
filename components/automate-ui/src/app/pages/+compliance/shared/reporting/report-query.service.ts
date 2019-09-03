import { Injectable } from '@angular/core';
import { BehaviorSubject } from 'rxjs';
import * as moment from 'moment';
import { FilterC } from '../../+reporting/types';
import { find, uniqWith, isEqual } from 'lodash';

export interface ReportQuery {
  startDate: Date;
  endDate: Date;
  interval: number;
  filters: FilterC[];
}

interface TimeIntervals {
  name: string;
  findStartDate: (endDate: Date) => Date;
}

@Injectable()
export class ReportQueryService {

  intervals: TimeIntervals[] = [
    { name: 'Last 10 days', findStartDate: (endDate: Date): Date =>
      moment(endDate).subtract(10, 'days').toDate()},
    { name: 'Last month', findStartDate: (endDate: Date): Date =>
      moment(endDate).subtract(1, 'months').toDate()},
    { name: 'Last 3 months', findStartDate: (endDate: Date): Date =>
      moment(endDate).subtract(3, 'months').toDate()},
    { name: 'Last year', findStartDate: (endDate: Date): Date =>
      moment(endDate).subtract(1, 'years').toDate()}
  ];

  state: BehaviorSubject<ReportQuery> = new BehaviorSubject(this.initialReportQueryState());

  private initialReportQueryState() {
    return {
      startDate: moment().subtract(10, 'days').toDate(),
      endDate: new Date(),
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

  addFilter(filter: FilterC) {
    const currentState = this.getReportQuery();

    currentState.filters = this.dedupFilters(currentState.filters, filter);

    this.state.next(currentState);
  }

  removeFilter(filter: FilterC) {
    const currentState = this.getReportQuery();
    currentState.filters.splice(currentState.filters.indexOf(filter), 1);

    this.state.next(currentState);
  }

  dedupFilters(filters: FilterC[], filter: FilterC) {
    // this is needed because the node and profile objects are slightly
    // different based on how they were added to the filters list
    const alreadyFilteredCheckID = find(filters, function(f: FilterC) {
      if (filter.type.name === 'profile' || filter.type.name === 'node') {
        if (f.value) {
          return f.type.name === filter.type.name &&
            f.value.id === filter.value.id;
        }
      }
    });
    if (alreadyFilteredCheckID === undefined) {
      filters.push(filter);
    }

    return uniqWith(filters, isEqual);
  }

  setFilters(newFilters: FilterC[]) {
    const currentState = this.getReportQuery();

    currentState.filters = newFilters;

    this.state.next(currentState);
  }

  setDateRange(startDate: Date, endDate: Date) {
    const currentState = this.getReportQuery();

    currentState.startDate = startDate;
    currentState.endDate = endDate;

    this.state.next(currentState);
  }

  findTimeIntervalStartDate(interval: number): Date {
    const currentState = this.getReportQuery();

    return this.intervals[interval].findStartDate(currentState.endDate);
  }

  setInterval(newInterval: number) {
    const currentState = this.getReportQuery();

    currentState.interval = newInterval;
    currentState.startDate = this.intervals[newInterval].findStartDate(currentState.endDate);

    this.state.next(currentState);
  }
}
