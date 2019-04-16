import { HttpClient, HttpParams } from '@angular/common/http';
import { Observable } from 'rxjs';
import { Injectable } from '@angular/core';

import {
  ServiceGroupsPayload, ServiceGroupFilters, ServicesPayload
} from './service-groups.model';
import { environment } from '../../../environments/environment';
const APPLICATIONS_URL = environment.applications_url;

@Injectable()
export class ServiceGroupsRequests {

  constructor(private httpClient: HttpClient) {}

  public fetchServiceGroups(filters?: ServiceGroupFilters): Observable<ServiceGroupsPayload> {
    const url = `${APPLICATIONS_URL}/service-groups`;

    const options = {
      params: this.buildFilterParams(filters)
    };

    return this.httpClient.get<ServiceGroupsPayload>(url, options);
  }

  public buildFilterParams(filters?: ServiceGroupFilters): HttpParams {
    let searchParam = new HttpParams();
    if (filters) {
      if (filters.status) {
        searchParam = searchParam.append('filter', 'status' + ':' + filters.status);
      }
      if (filters.sortField) {
        searchParam = searchParam.append('sorting.field', filters.sortField);
      }
      if (filters.sortDirection) {
        searchParam = searchParam.append('sorting.order', filters.sortDirection);
      }
      if (filters.page) {
        searchParam = searchParam.append('pagination.page', filters.page.toString());
      }
      if (filters.pageSize) {
        searchParam = searchParam.append('pagination.size', filters.pageSize.toString());
      }
    }

    return searchParam;
  }

  public fetchServicesBySG(serviceGroupId?: number): Observable<ServicesPayload> {
    const url = `${APPLICATIONS_URL}/service-groups/${serviceGroupId}`;

    return this.httpClient.get<ServicesPayload>(url);
  }
}
