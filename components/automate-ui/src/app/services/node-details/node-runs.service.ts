import { Injectable } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';

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

  getPolicyCookbooks(revision_id: string): Promise<PolicyCookbooks> {
    const url = `${CONFIG_MGMT_URL}/policy_revision/${revision_id}`;

    return this.httpClient
      .get<RespPolicyCookbooks>(url).toPromise()
      .then((res) => new PolicyCookbooks(res));
  }

  getNodeRun(nodeId: string, runId: string, endTime: Date): Promise<NodeRun> {
    const url = `${CONFIG_MGMT_URL}/nodes/${nodeId}/runs/${runId}`;
    const searchParam = new HttpParams().set('end_time', endTime.toISOString());

    const options = {
      params: searchParam
    };

    return this.httpClient
      .get<RespNodeRun>(url, options).toPromise()
      .then((res) => new NodeRun(this.formatError(res)));
  }

  // This method cleans up an error message comming from chef server.
  // Because 412 "Precondition Failed" is not user friendly, the message was changed to
  // "Error Resolving Cookbooks for Run List."
  //
  // TODO remove to the backend. This could be moved to the ingest service.
  private formatError(respNodeRun: RespNodeRun): RespNodeRun {
    if (this.isNullErrorDescription(respNodeRun)) {
        // remove confusing colon at end of cookbook depsolve error
        if ( respNodeRun.error.description.title === '412 "Precondition Failed"' ||
        respNodeRun.error.description.title === 'Error Resolving Cookbooks for Run List:' ) {
          respNodeRun.error.description.title = 'Error Resolving Cookbooks for Run List.';
        }
    }
    return respNodeRun;
  }

  private isNullErrorDescription(respNodeRun: RespNodeRun): boolean {
    return respNodeRun != null &&
      respNodeRun.error != null &&
      respNodeRun.error.description != null;
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
    let searchParam = new HttpParams().set('filter', `node_id:${filters.nodeId}`);

    if (filters.startDate) {
      searchParam = searchParam.set('start', filters.startDate);
    }

    if (filters.endDate) {
      searchParam = searchParam.set('end', filters.endDate);
    }

    return searchParam;
  }
}
