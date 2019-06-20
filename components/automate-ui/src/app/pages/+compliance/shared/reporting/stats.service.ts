import { map } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import * as moment from 'moment';
import { omitBy, isNil } from 'lodash';
import { environment } from '../../../../../environments/environment';

const CC_API_URL = environment.compliance_url;

// Types
export interface DateRange {
  start: Date | moment.Moment;
  end: Date | moment.Moment;
}

@Injectable()
export class StatsService {
  constructor(
    private httpClient: HttpClient
  ) {}

  getFailures(types: Array<string>, filters): Observable<any> {
    const url = `${CC_API_URL}/reporting/stats/failures`;
    const formatted = this.formatFilters(filters);
    formatted.push({ type: 'types', values: types });
    const body = { filters: formatted };

    return this.httpClient.post<any>(url, body);
  }

  getNodeSummary(filters): Observable<any> {
    const url = `${CC_API_URL}/reporting/stats/summary`;
    const formatted = this.formatFilters(filters);
    const body = { type: 'nodes', filters: formatted };

    return this.httpClient.post<any>(url, body).pipe(
      map(({ node_summary }) => node_summary));
  }

  getControlsSummary(filters): Observable<any> {
    const url = `${CC_API_URL}/reporting/stats/summary`;
    const formatted = this.formatFilters(filters);
    const body = { type: 'controls', filters: formatted };

    return this.httpClient.post<any>(url, body).pipe(
      map(({ controls_summary }) => controls_summary));
  }

  getNodeTrend(filters) {
    const url = `${CC_API_URL}/reporting/stats/trend`;
    const interval = 86400;

    const formatted = this.formatFilters(filters);
    const body = {type: 'nodes', interval, filters: formatted};

    return this.httpClient.post<any>(url, body).pipe(
      map(({ trends }) => trends));
  }

  getControlsTrend(filters) {
    const url = `${CC_API_URL}/reporting/stats/trend`;
    const interval = 86400;

    const formatted = this.formatFilters(filters);
    const body = {type: 'controls', interval, filters: formatted};

    return this.httpClient.post<any>(url, body).pipe(
      map(({ trends }) => trends));
  }

  getSummary(filters: any[]) {
    const url = `${CC_API_URL}/reporting/stats/summary`;
    const body = { filters: this.formatFilters(filters) };

    return this.httpClient.post<any>(url, body).pipe(
      map(({ report_summary }) => report_summary));
  }

  getProfileResultsSummary(id: string, filters) {
    const url = `${CC_API_URL}/reporting/stats/profiles`;
    const body = {type: 'summary', id, filters: this.formatFilters(filters)};

    return this.httpClient.post<any>(url, body).pipe(
      map(({ profile_summary }) => profile_summary));
  }

  getProfileResults(id: string, filters) {
    const url = `${CC_API_URL}/reporting/stats/profiles`;
    const body = {type: 'controls', id, filters: this.formatFilters(filters)};

    return this.httpClient.post<any>(url, body).pipe(
      map(({ control_stats }) => control_stats.map(c => {
        return {passed: 0, failed: 0, skipped: 0, ...c};
      })));
  }

  getNodes(filters: any[], listParams: any): Observable<any> {
    const url = `${CC_API_URL}/reporting/nodes/search`;

    const formatted = this.formatFilters(filters);
    let body = { filters: formatted };

    const {page, perPage} = listParams;
    if (page && perPage) {
      body = Object.assign(body, {page, per_page: perPage});
    }

    const {sort, order} = listParams;
    if (sort && order) {
      body = Object.assign(body, {sort, order: order.toUpperCase()});
    }

    return this.httpClient.post<any>(url, body).pipe(
      map(({ nodes, total }) => ({total, items: nodes})));
  }

  getProfiles(filters: any[], listParams: any): Observable<any> {
    const url = `${CC_API_URL}/reporting/profiles`;

    const formatted = this.formatFilters(filters);
    let body = { filters: formatted };

    const {page, perPage} = listParams;
    if (page && perPage) {
      body = Object.assign(body, {page, per_page: perPage});
    }

    const {sort, order} = listParams;
    if (sort && order) {
      body = Object.assign(body, {sort, order: order.toUpperCase()});
    }

    return this.httpClient.post<any>(url, body).pipe(
      map(({ profiles, counts: { total }}) => ({total, items: profiles})));
  }

  getStatsProfiles(filters: any[], listParams: any): Observable<any> {
    const url = `${CC_API_URL}/reporting/stats/profiles`;

    const formatted = this.formatFilters(filters);
    let body = { filters: formatted };

    const {page, perPage} = listParams;
    if (page && perPage) {
      body = Object.assign(body, {page, per_page: perPage});
    }

    const {sort, order} = listParams;
    if (sort && order) {
      body = Object.assign(body, {sort, order: order.toUpperCase()});
    }

    return this.httpClient.post<any>(url, body).pipe(
      map(({ profile_list, total }) => {
        return {total, items: profile_list};
      }));
  }

  getReports(filters: any[], listParams: any): Observable<any> {
    const url = `${CC_API_URL}/reporting/reports`;
    const formatted = this.formatFilters(filters);
    let body = { filters: formatted };

    const {sort, order} = listParams;
    if (sort && order) {
      body = Object.assign(body, {sort, order: order.toUpperCase()});
    }

    return this.httpClient.post<any>(url, body).pipe(
      map(({ reports }) => reports));
  }

  downloadReport(format: string, filters: any[]): Observable<string> {
    const url = `${CC_API_URL}/reporting/export`;

    filters = this.formatFilters(filters)

    // for export, we want to send the start_time as the beg of day of end time
    // so we find the endtime in the filters, and then set start time to beg of that day
    const filtersCopy = [];
    let endtime = filters.find(function (filt) {
      return filt["type"] == "end_time"
    })
    filters.forEach(function (filt) {
      if (filt["type"] == "start_time") {
        let val = moment(endtime["values"][0]).startOf('day')
        filt["values"] = [val.format('YYYY-MM-DDTHH:mm:ssZ')]
      }
      filtersCopy.push(filt)
    })

    const body = { type: format,
                   filters: filtersCopy};
                   
    return this.httpClient.post(url, body, {responseType: 'text'});
  }

  getSingleReport(reportID: string, filters: any[]): Observable<any> {
    const url = `${CC_API_URL}/reporting/reports/id/${reportID}`;
    const formatted = this.formatFilters(filters);
    const body = { filters: formatted };

    return this.httpClient.post<any>(url, body).pipe(
      map((data) => {
        data.profiles.forEach(p => {
          p.controls.forEach(c => {
            // precalculate overall control status from results
            c.results = c.results || [];
            c.status = this.getControlStatus(c);
          });
        });
        return omitBy(data, isNil);
      }));
  }

  /* TODO Auth team: Helper functions that are only not private because
     they have unit testing. Should we delete their tests and make
     them private? */
  getControlStatus(control): string {
    const statuses = control.results.map(r => r.status);
    if (statuses.every(s => s === 'passed')) { return 'passed'; }
    if (statuses.every(s => s === 'skipped')) { return 'skipped'; }
    return 'failed';
  }

  addDateRange(filters, dateRange) {
    if (filters) {
      filters.push(
        {'end_time': moment(dateRange.end).format('YYYY-MM-DDTHH:mm:ssZ')},
        {'start_time': moment(dateRange.start).format('YYYY-MM-DDTHH:mm:ssZ')}
      );
    }
    return filters;
  }

  formatFilters(filters) {
    return filters.reduce((formatted, filter) => {
      if (filter['end_time'] || filter['start_time']) {
        const type = Object.keys(filter)[0];
        const now = moment();
        let value = moment(filter[type])
          .set({
            hour: now.get('hour'),
            minute: now.get('minute'),
            second: now.get('second')
          })
          .utc();

        if (type === 'end_time') {
          value = value.endOf('day');
        } else {
          value = value.startOf('day');
        }

        const values = [value.format()];
        formatted.push({type, values});
      } else {
        let type = filter['type']['name'];
        let value = filter['value']['id'] || filter['value']['text'];
        if (type === 'profile') {
            type = 'profile_id';
          // profile needs to be sent as profile_id if we have the ID
          // and 'profile_name' for wildcard filters
          if ( filter['value']['id'] ) {
            type = 'profile_id';
            value = filter['value']['id'];
          } else {
            type = 'profile_name';
            value = filter['value']['text'];
          }
        } else if (type === 'node') {
          // node needs to be sent as node_id if we have the ID
          // and 'node_name' for wildcard filters
          if ( filter['value']['id'] ) {
            type = 'node_id';
            value = filter['value']['id'];
          } else {
            type = 'node_name';
            value = filter['value']['text'];
          }
        } else if (type === 'control') {
          // control needs to be sent as 'control' if we have the ID
          // and 'control_name' for wildcard filters
          if ( filter['value']['id'] ) {
            type = 'control';
            value = filter['value']['id'];
          } else {
            type = 'control_name';
            value = filter['value']['text'];
          }
        }

        const group = formatted.filter(f => f.type === type)[0];

        if (group) {
          group.values.push(value);
        } else {
          formatted.push({type, values: [value]});
        }
      }
      return formatted;
    }, []);
  }
}
