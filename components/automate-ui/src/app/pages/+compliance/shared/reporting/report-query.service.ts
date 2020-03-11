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

interface ReportSummary {
  end_time: string;
}

@Injectable()
export class ReportQueryService {

  intervals: TimeIntervals[] = [
    { name: 'Last 10 days', findStartDate: (endDate: moment.Moment): moment.Moment =>
      endDate.clone().subtract(10, 'days')},
    { name: 'Last month', findStartDate: (endDate: moment.Moment): moment.Moment =>
      endDate.clone().subtract(1, 'months')},
    { name: 'Last 3 months', findStartDate: (endDate: moment.Moment): moment.Moment =>
      endDate.clone().subtract(3, 'months')},
    { name: 'Last year', findStartDate: (endDate: moment.Moment): moment.Moment =>
      endDate.clone().subtract(1, 'years')}
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
    const reportQuery = this.state.getValue();
    return {
      startDate: reportQuery.startDate.clone(),
      endDate: reportQuery.endDate.clone(),
      interval: reportQuery.interval,
      filters: [...reportQuery.filters]
    };
  }

  // getReportQueryForReport provides the same functionality as getReportQuery,
  // but uses the additional information in the given report summary to make
  // the query more efficient for the backend server.
  getReportQueryForReport(report: ReportSummary): ReportQuery {
    // Reports are stored separately (per-day) according to their `end_time` on
    // the server; therefore, the API to fetch a report is much faster/uses
    // fewer resources when we limit the date/time range of our query to the
    // minimum possible. We use the `end_time` we already have from the report
    // to restrict our query to a very narrow time range. The StatsService will
    // take care of setting the start and end dates to the beginning and end of
    // the given day as needed.
    const query = this.getReportQuery();
    query.startDate = moment(report.end_time);
    query.endDate = moment(report.end_time);
    return query;
  }

  setState(newState: ReportQuery) {
    this.state.next(newState);
  }

  findTimeIntervalStartDate(interval: number, endDate: moment.Moment): moment.Moment {
    return this.intervals[interval].findStartDate(endDate.clone());
  }

  setFilterTitle(type: string, id: string, title: string) {
    this.idToTitle.set(type + '-' + id, title);
  }

  getFilterTitle(type: string, id: string): string {
    return this.idToTitle.get(type + '-' + id);
  }
}
