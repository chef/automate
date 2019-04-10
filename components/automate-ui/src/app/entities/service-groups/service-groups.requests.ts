import { HttpClient, HttpParams } from '@angular/common/http';
import { Observable } from 'rxjs';
import { Injectable } from '@angular/core';

import {
  ServiceGroupsPayload, ServiceGroupFilters
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
    }

    return searchParam;
  }
}
