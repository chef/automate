import { map } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { Observable, of as observableOf } from 'rxjs';
import { reduce, includes, union } from 'lodash/fp';
import { Chicklet, NodeCount } from '../../types/types';
import {
  NodeFilter
} from './client-runs.model';

import {
  RespServiceVersions,
  RespService,
  RespNode,
  Node
} from './client-runs.model';

import { environment } from '../../../environments/environment';
const CONFIG_MGMT_URL = environment.config_mgmt_url;
const INGEST_URL = environment.ingest_url;
const DEPLOYMENT_URL = environment.deployment_url;

interface RespSuggestion {
  text: string;
}

@Injectable()
export class ClientRunsRequests {
  constructor(
    private httpClient: HttpClient
  ) {}

  public downloadNodes(type: string, filters: NodeFilter): Observable<string> {
    const url = `${CONFIG_MGMT_URL}/nodes/export`;

    const body = {
      output_type: type,
      filter: this.buildFilters(filters),
      sorting: {}
    };

    if (filters.sortField) {
      body.sorting['field'] = this.serviceSortField(filters.sortField);
    }

    if (filters.sortDirection) {
      if (filters.sortDirection.toLowerCase() === 'desc') {
        body.sorting['order'] = 1;
      } else {
        body.sorting['order'] = 0;
      }
    }

    return this.httpClient.post(url, body, {responseType: 'text'});
  }

  public deleteNodes(nodeIds: string[]): Observable<boolean> {
    const url = `${INGEST_URL}/events/chef/node-multiple-deletes`;
    const body = {node_ids: nodeIds};
    return this.httpClient.post<any>(url, body).pipe(map((_res) => true));
  }

  // TODO: need to find a better place for this request.
  public isWorkflowEnabled(): Observable<boolean> {
    const url = `${DEPLOYMENT_URL}/service_versions`;

    return this.httpClient.get<any>(url).pipe(
      map((res: RespServiceVersions) =>
      res.services.find((service: RespService) =>
        service.name === 'automate-workflow-server') !== undefined ));
  }

  public getNodeCount(filters?: NodeFilter): Observable<NodeCount> {
    const url = `${CONFIG_MGMT_URL}/stats/node_counts`;
    const options = filters ? { params: this.formatFilters(filters) } : {};

    return this.httpClient
      .get<NodeCount>(url, options);
  }

  public getSuggestions(type: string, text: string, filters: NodeFilter): Observable<any[]> {
    if (text && text.length > 0) {
      const params = this.formatFilters(filters).set('type', type).set('text', text);
      const url = `${CONFIG_MGMT_URL}/suggestions`;

      return this.httpClient.get<RespSuggestion[]>(url, {params}).pipe(map(
        (suggestions) => suggestions.filter(s => s && s.text && s.text.length !== 0)));
    } else {
      return observableOf([]);
    }
  }

  public getNodes(filters?): Observable<Node[]> {
    const url = `${CONFIG_MGMT_URL}/nodes`;

    const options = {
      params: this.buildURLSearchParams(filters)
    };

    return this.httpClient
      .get<RespNode[]>(url, options).pipe(
      map((res) => res.map((respNode: RespNode) => this.createNode(respNode))));
  }

  public buildURLSearchParams(filters?): HttpParams {
    let searchParam = new HttpParams();

    if (filters) {
      if (filters.page) {
        searchParam = searchParam.append('pagination.page', filters.page);
      }

      if (filters.pageSize) {
        searchParam = searchParam.append('pagination.size', filters.pageSize);
      }

      if (filters.sortField) {
        searchParam = searchParam.append('sorting.field', this.serviceSortField(filters.sortField));
      }

      if (filters.sortDirection) {
        searchParam = searchParam.append('sorting.order', filters.sortDirection);
      }

      if (filters.searchBar) {
        searchParam = reduce((param, pill) => {
          const filterParam = `${encodeURIComponent(pill.type)}:${encodeURIComponent(pill.text)}`;
          return param.append('filter', filterParam);
        }, searchParam, filters.searchBar);
      }
      if (filters.status) {
        searchParam = searchParam.append('filter', 'status' + ':' + filters.status);
      }
      if (filters.nodeId) {
        searchParam = searchParam.append('filter', 'node_id' + ':' + filters.nodeId);
      }
    }

    return searchParam;
  }

  private buildFilters(nodeFilter: NodeFilter): string[] {
    let filters: string[] = [];
    if (nodeFilter.searchBar) {
      const searchBarFilters = reduce((arr: string[], chicklet: Chicklet) => {
        const filterParam =
        `${encodeURIComponent(chicklet.type)}:${encodeURIComponent(chicklet.text)}`;
        return union(arr, [filterParam]);
      }, [], nodeFilter.searchBar);

      filters = filters.concat(searchBarFilters);
    }

    if (nodeFilter.status) {
      filters.push('status:' + nodeFilter.status);
    }

    return filters;
  }

  private createNode(respNode: RespNode): Node {
    return {
      id: respNode.id,
      name: respNode.name,
      status: respNode.status,
      checkin: respNode.checkin,
      uptime_seconds: respNode.uptime_seconds,
      platform: respNode.platform,
      environment: respNode.environment,
      policy_group: respNode.policy_group,
      latestRunId: respNode.latest_run_id,
      fqdn: respNode.fqdn,
      organization: respNode.organization,
      source_fqdn: respNode.source_fqdn,
      hasRuns: respNode.has_runs_data,
      lastCcrReceived: respNode.last_ccr_received,
      deprecationsCount: respNode.deprecations_count,
      chefVersion: respNode.chef_version
    };
  }

  private serviceSortField(field: string): string {
    const validSortFields = [
      'name',
      'checkin',
      'uptime_seconds',
      'platform',
      'environment',
      'policy_group',
      'chef_version',
      'deprecations_count'
    ];

    if (!includes(field, validSortFields)) {
      console.log('Invalid field to sort on: ' , field , ', defaulting to name');
      field = 'name';
    }
    return field;
  }

  private formatFilters(filters: NodeFilter) {
    let searchParam = new HttpParams();

    if (filters.searchBar) {
      searchParam = this.flattenSearchBar(filters.searchBar, searchParam);
    }

    return searchParam;
  }

  private flattenSearchBar(filters: object[], searchParam: HttpParams): HttpParams {
    return reduce((params: HttpParams, filter: { type: string, text: string }) => {
      const filterParam = `${encodeURIComponent(filter.type)}:${encodeURIComponent(filter.text)}`;
      return params.append('filter', filterParam);
    }, searchParam, filters);
  }
}
