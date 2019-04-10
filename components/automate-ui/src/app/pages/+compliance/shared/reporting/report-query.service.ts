import { Injectable } from '@angular/core';
import { BehaviorSubject } from 'rxjs';
import { FilterC } from '../../+reporting/types';
import { find, uniqWith, isEqual } from 'lodash';
import * as moment from 'moment';

@Injectable()
export class ReportQueryService {

  constructor() {
    const {intervals, interval} = this;
    const endDate = new Date();
    const startDate = intervals[interval][1](endDate);
    this.setDateRange(startDate, endDate);
  }

  startDate: Date = moment().subtract(10, 'days').toDate();

  endDate: Date = new Date();

  filters: BehaviorSubject<Array<any>> = new BehaviorSubject([]);

  intervals: [string, (date: Date) => Date][] = [
    ['Last 10 days', date => moment(date).subtract(10, 'days').toDate()],
    ['Last month', date => moment(date).subtract(1, 'months').toDate()],
    ['Last 3 months', date => moment(date).subtract(3, 'months').toDate()],
    ['Last year', date => moment(date).subtract(1, 'years').toDate()]
  ];

  interval = 0;

  addFilter(filter) {
    const filters = this.filters.getValue();
    if (filter.type.name === 'profile') {
      // append version to text to ensure we display version of profile
      filter.value.text = `${filter.value.text}, v${filter.value.version}`;
    }

    const dedupedFilters = this.dedupFilters(filters, filter);
    this.filters.next(dedupedFilters);
  }

  dedupFilters(filters, filter) {
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

  setDateRange(startDate, endDate) {
    this.startDate = startDate;
    this.endDate = endDate;
    const filters = this.filters.getValue()
      .filter(f => !f['end_time'] && !f['start_time'])
      .concat([
        {'start_time': startDate},
        {'end_time': endDate}
      ]);
    this.filters.next(filters);
  }

  removeFilter(filter) {
    const filters = this.filters.getValue();
    filters.splice(filters.indexOf(filter), 1);
    this.filters.next(filters);
  }

  clearFilters() {
    const filters = this.filters.getValue().filter(f => f['end_time'] || f['start_time']);
    this.filters.next(filters);
  }
}
