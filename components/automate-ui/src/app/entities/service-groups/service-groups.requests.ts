import { HttpClient, HttpParams } from '@angular/common/http';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { reduce } from 'lodash/fp';
import { Injectable } from '@angular/core';

import {
  ServiceGroupsPayload,
  ServiceGroupsFilters,
  ServiceGroupsSuggestions,
  ServiceGroupsHealthSummary,
  GroupServicesPayload,
  GroupServicesFilters,
  ServicesStats,
  GroupService
} from './service-groups.model';
import { Chicklet } from 'app/types/types';
import { environment } from 'environments/environment';
const APPLICATIONS_URL = environment.applications_url;

@Injectable()
export class ServiceGroupsRequests {

  constructor(private httpClient: HttpClient) {}

  public fetchServiceGroups(filters?: ServiceGroupsFilters): Observable<ServiceGroupsPayload> {
    const url = `${APPLICATIONS_URL}/service-groups`;

    const options = {
      params: this.buildServiceGroupsFilterParams(filters)
    };

    return this.httpClient.get<ServiceGroupsPayload>(url, options);
  }

  public buildServiceGroupsFilterParams(filters?: ServiceGroupsFilters): HttpParams {
    let params = new HttpParams();

    if (filters) {
      if (filters.status) {
        params = params.append('filter', 'status' + ':' + filters.status);
      }
      if (filters.sortField) {
        params = params.append('sorting.field', filters.sortField);
      }
      if (filters.sortDirection) {
        params = params.append('sorting.order', filters.sortDirection);
      }
      if (filters.page) {
        params = params.append('pagination.page', filters.page.toString());
      }
      if (filters.pageSize) {
        params = params.append('pagination.size', filters.pageSize.toString());
      }
      if (filters.searchBar) {
        params = reduce((param, pill) => {
          const filterParam = `${encodeURIComponent(pill.type)}:${encodeURIComponent(pill.text)}`;
          return param.append('filter', filterParam);
        }, params, filters.searchBar);
      }
    }

    return params;
  }

  public fetchServicesBySG(filters?: GroupServicesFilters): Observable<GroupServicesPayload> {
    const url = `${APPLICATIONS_URL}/service-groups/${filters.service_group_id}`;

    const options = {
      params: this.buildServicesBySGFilterParams(filters)
    };
    return this.httpClient.get<GroupServicesPayload>(url, options);
  }

  public buildServicesBySGFilterParams(filters?: GroupServicesFilters): HttpParams {
    let params = new HttpParams();
    if (filters) {
      if (filters.health && filters.health !== 'total') {
       params = params.append('filter', `status:${filters.health}`);
      }
      if (filters.page && filters.pageSize) {
        params = params.append('pagination.page', filters.page.toString());
        params = params.append('pagination.size', filters.pageSize.toString());
      }
      if (filters.searchBar) {
        params = reduce((param, pill) => {
          const filterParam = `${encodeURIComponent(pill.type)}:${encodeURIComponent(pill.text)}`;
          return param.append('filter', filterParam);
        }, params, filters.searchBar);
      }
    }

    return params;
  }

  public fetchServiceGroupHealth(filters: ServiceGroupsFilters):
    Observable<ServiceGroupsHealthSummary> {
      const url = `${APPLICATIONS_URL}/service_groups_health_counts`;
      const params = this.formatFilters(filters);

      return this.httpClient.get<ServiceGroupsHealthSummary>(url, {params});
  }

  public getServiceStats(): Observable<ServicesStats> {
    const url = `${APPLICATIONS_URL}/stats`;

    return this.httpClient.get<ServicesStats>(url).pipe(
      map(
      (stats: any) => ({
        totalServiceGroups: stats.total_service_groups,
        totalServices: stats.total_services,
        totalSupervisors: stats.total_supervisors,
        totalDeployments: stats.total_deployments
      })
    ));
  }

  public getSuggestions(
    fieldName: string,
    queryFragment: string,
    filters: ServiceGroupsFilters): Observable<any[]> {

      // This api requires different filter names for group and service, should it?
      fieldName =
          fieldName === 'group' ? 'group_name'
        : fieldName === 'service' ? 'service_name'
        : fieldName;
      const params = this.formatFilters(filters)
        .set('field_name', fieldName)
        .set('query_fragment', queryFragment);
      const url = `${APPLICATIONS_URL}/services-distinct-values`;

      return this.httpClient.get<ServiceGroupsSuggestions>(url, {params}).pipe(map(
        (suggestions) => suggestions.values));
  }

  private formatFilters(filters: ServiceGroupsFilters) {
    let searchParam = new HttpParams();

    if (filters.searchBar) {
      searchParam = this.flattenSearchBar(filters.searchBar, searchParam);
    }

    return searchParam;
  }

  private flattenSearchBar(filters: Chicklet[], searchParam: HttpParams): HttpParams {
    return reduce((params: HttpParams, filter: Chicklet) => {
      const filterParam = `${encodeURIComponent(filter.type)}:${encodeURIComponent(filter.text)}`;
      return params.append('filter', filterParam);
    }, searchParam, filters);
  }

  public deleteServicesById(listOfIds: number[]): Observable<any> {
    const url = `${APPLICATIONS_URL}/delete_services_by_id`;

    const params = { ids: listOfIds };
    return this.httpClient.post<GroupService[]>(url, params);
  }
}
