import { Injectable } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { Observable } from 'rxjs';

import {
  NodeHistoryFilter,
  NodeHistoryCountsFilter,
  AbridgedNodeRun,
  AbridgedRespNodeRun,
  NodeRun,
  NodeRunsCount,
  RespNodeRunsCount,
  RespNodeRun,
  RespPolicyCookbooks,
  PolicyCookbooks } from '../../types/types';

import { environment } from '../../../environments/environment';
import * as moment from 'moment/moment';
const CONFIG_MGMT_URL = environment.config_mgmt_url;

@Injectable()
export class NodeRunsService {
  constructor(
    private httpClient: HttpClient
  ) {}

  getNodeRunCounts(filters: NodeHistoryCountsFilter): Promise<NodeRunsCount> {
    const url = `${CONFIG_MGMT_URL}/stats/run_counts`;

    const options = {
      params: this.buildURLSearchParamsNodeCounts(filters)
    };

    return this.httpClient
      .get<RespNodeRunsCount>(url, options).toPromise()
      .then((res) => new NodeRunsCount(res))
      .catch( reason => {
        console.error(reason);
        return new NodeRunsCount({total: 0, success: 0, failure: 0});
      });
  }

  getNodeRuns(filters: NodeHistoryFilter): Promise<AbridgedNodeRun[]> {
    const url = `${CONFIG_MGMT_URL}/nodes/${filters.nodeId}/runs`;

    const options = {
      params: this.buildURLSearchParams(filters)
    };

    return this.httpClient
      .get<AbridgedRespNodeRun[]>(url, options).toPromise()
      .then((res) =>
        res.map(
          (abridgedRespNodeRun: AbridgedRespNodeRun) =>
            new AbridgedNodeRun(abridgedRespNodeRun))
      ).catch( reason => {
        console.error(reason);
        return [];
      });
  }

  getNodeRunsByID(nodeId: string): Promise<AbridgedNodeRun[]> {
    const url = `${CONFIG_MGMT_URL}/nodes/${nodeId}/runs`;

    return this.httpClient
      .get<AbridgedRespNodeRun[]>(url).toPromise()
      .then((res) =>
        res.map(
          (abridgedRespNodeRun: AbridgedRespNodeRun) =>
            new AbridgedNodeRun(abridgedRespNodeRun))
      ).catch( reason => {
        console.error(reason);
        return [];
      });
  }

  downloadRuns(type: string, filters: NodeHistoryFilter): Observable<string> {
    const url = `${CONFIG_MGMT_URL}/reports/export`;

    let typeFilters: string[] = [];
    if (filters.status) {
      typeFilters = [`status:${filters.status}`];
    }

    const body = {
      output_type: type,
      sorting: {},
      node_id: filters.nodeId,
      filter: typeFilters,
      start: {
        seconds: moment.utc(filters.startDate).unix()
      },
      end: {
        seconds: moment.utc().endOf('day').unix()
      }
    };

    return this.httpClient.post(url, body, {responseType: 'text'});
  }

  getPolicyCookbooks(revision_id: string): Promise<PolicyCookbooks> {
    const url = `${CONFIG_MGMT_URL}/policy_revision/${revision_id}`;

    return this.httpClient
      .get<RespPolicyCookbooks>(url).toPromise()
      .then((res) => new PolicyCookbooks(res));
  }

  getNodeRun(nodeId: string, runId: string, endTime?: Date): Promise<NodeRun> {
    const url = `${CONFIG_MGMT_URL}/nodes/${nodeId}/runs/${runId}`;
    let searchParam = new HttpParams();

    if (endTime) {
      searchParam = searchParam.set('end_time', endTime.toISOString());
    }

    const options = {
      params: searchParam
    };

    return this.httpClient
      .get<RespNodeRun>(url, options).toPromise()
      .then((res) => new NodeRun(res));
  }

  private buildURLSearchParams(filters: NodeHistoryFilter): HttpParams {
    let searchParam = new HttpParams()
      .set('pagination.page', String(filters.page))
      .set('pagination.size', String(filters.pageSize));

    if (filters.startDate) {
      searchParam = searchParam.set('start', filters.startDate);
    }

    if (filters.status) {
      searchParam = searchParam.set('filter', `status:${filters.status}`);
    }

    if (filters.endDate) {
      searchParam = searchParam.set('end', filters.endDate);
    }

    return searchParam;
  }

  // /stats/run_counts?node_id=e4ad572b-30b7-4806-aebb-43e1598e3a08&start_time=2017-09-08T14:03:05Z
  private buildURLSearchParamsNodeCounts(filters: NodeHistoryCountsFilter): HttpParams {
    let searchParam = new HttpParams().set('node_id', filters.nodeId);

    if (filters.startDate) {
      searchParam = searchParam.set('start', filters.startDate);
    }

    if (filters.endDate) {
      searchParam = searchParam.set('end', filters.endDate);
    }

    return searchParam;
  }
}
