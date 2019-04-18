import { HttpClient, HttpParams } from '@angular/common/http';
import { Observable } from 'rxjs';
import { Injectable } from '@angular/core';

import {
  ServiceGroupsPayload, ServiceGroupFilters, ServicesPayload, ServicesFilters
} from './service-groups.model';
import { environment } from '../../../environments/environment';
const APPLICATIONS_URL = environment.applications_url;

@Injectable()
export class ServiceGroupsRequests {

  constructor(private httpClient: HttpClient) {}

  public fetchServiceGroups(filters?: ServiceGroupFilters): Observable<ServiceGroupsPayload> {
    const url = `${APPLICATIONS_URL}/service-groups`;

    const options = {
      params: this.buildServiceGroupsFilterParams(filters)
    };

    return this.httpClient.get<ServiceGroupsPayload>(url, options);
  }

  public buildServiceGroupsFilterParams(filters?: ServiceGroupFilters): HttpParams {
    let params = new HttpParams();

    if (filters) {
      if (filters.status) {
        params = params.append('filter', 'status' + ':' + filters.status);
      }
    }

    return params;
  }

  public fetchServicesBySG(filters?: ServicesFilters): Observable<ServicesPayload> {
    const url = `${APPLICATIONS_URL}/service-groups/${filters.service_group_id}`;

    const options = {
      params: this.buildServicesBySGFilterParams(filters)
    };

    return this.httpClient.get<ServicesPayload>(url, options);
  }

  public buildServicesBySGFilterParams(filters?: ServicesFilters): HttpParams {
    let params = new HttpParams();

    if (filters) {
      if (filters.health && filters.health !== 'total') {
        params = params.append('health', filters.health);
      }
    }

    return params;
  }
}
