import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { HttpClient } from '@angular/common/http';
import { mapKeys, snakeCase } from 'lodash/fp';

import { environment as env } from '../../../environments/environment';
import { Manager } from './manager.model';
import {
  ManagersSearchPayload,
  ManagerGetNodesPayload,
  ManagerSearchNodesPayload,
  ManagerSearchFieldsPayload
} from './manager.actions';
import { TelemetryService } from '../../services/telemetry/telemetry.service';

export interface ManagersSearchResponse {
  managers: Manager[];
  total: number;
}

export interface ManagerGetNodesResponse {
  nodes: any[];
  total: number;
}

export interface ManagerSearchNodesResponse {
  nodes: any[];
  total: number;
}

export interface ManagerSearchFieldsResponse {
  fields: string[];
}

@Injectable()
export class ManagerRequests {

  constructor(private http: HttpClient, private telemetryService: TelemetryService) { }

  public search(payload: ManagersSearchPayload): Observable<ManagersSearchResponse> {
    const url = `${env.nodemgrs_url}/search`;
    return this.http.post<ManagersSearchResponse>(url, payload);
  }

  public searchNodes(payload: ManagerSearchNodesPayload):
    Observable<ManagerSearchNodesResponse> {
      const url = `${env.nodemgrs_url}/id/${payload.managerId}/search-nodes`;
      const body = payload.query;
      return this.http.post<ManagerSearchNodesResponse>(url, body);
  }

  public getNodes(payload: ManagerGetNodesPayload):
    Observable<any> {
      const url = `${env.nodes_url}/search`;
      const { managerId, page, per_page } = payload;
      const filters = [{ key: 'manager_id', values: [managerId] }];
      const body = { filters, page, per_page };
      return this.http.post(url, body);
  }

  public deleteNodes(nodeIds: string[]): Observable<boolean> {
    const url = `${env.nodes_url}/delete/ids`;
    const body = { ids: nodeIds };
    return this.http.post<any>(url, body);
  }

  public searchFields(payload: ManagerSearchFieldsPayload):
    Observable<ManagerSearchFieldsResponse> {
      const url = `${env.nodemgrs_url}/id/${payload.managerId}/search-fields`;
      const body = {query: {filter_map: []}, field: payload.field};
      return this.http.post<ManagerSearchFieldsResponse>(url, body);
  }

  public fetch({id}) {
    return this.http.get<Manager>(`${env.nodemgrs_url}/id/${id}`);
  }

  public delete({id}) {
    return this.http.delete(`${env.nodemgrs_url}/id/${id}`);
  }

  public create(managerData) {
    const managerType = managerData['type'];
    this.telemetryService.track('nodeManagerType', { managerType });
    return this.http.post<Manager>(`${env.nodemgrs_url}`, mapKeys(snakeCase, managerData));
  }

  public update(managerData) {
    return this.http.put<Manager>(`${env.nodemgrs_url}/id/${managerData.id}`,
                                  mapKeys(snakeCase, managerData));
  }
}
