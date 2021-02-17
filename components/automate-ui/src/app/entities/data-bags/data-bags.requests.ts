import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { DataBagsSuccessPayload } from './data-bags.actions';
import { DataBagSearchPayload, DataBagItemListsSuccessPayload } from './data-bag-details.actions';
import { DataBagsItemDetails } from './data-bags.model';
import { InterceptorSkipHeader } from 'app/services/http/http-client-auth.interceptor';

const headers = new HttpHeaders().set(InterceptorSkipHeader, '');

export interface DataBagSearchResponse {
  items: any[];
  total: number;
}

@Injectable()
export class DataBagsRequests {

  constructor(private http: HttpClient) { }

  public getDataBags(server_id: string, org_id: string): Observable<DataBagsSuccessPayload> {
    return this.http.get<DataBagsSuccessPayload>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/data_bags`, {headers});
  }

  public getDataBagItemList(server_id: string, org_id: string, name: string)
  : Observable<DataBagItemListsSuccessPayload> {
    return this.http.get<DataBagItemListsSuccessPayload>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/data_bags/${name}`,
      {headers}
    );
  }

  public getDataBagSearchDetails(payload: DataBagSearchPayload)
  : Observable<DataBagSearchResponse> {
    const nameTarget = payload.databagName + '*';
    const params = `search_query.q=id:${nameTarget}*&search_query.page=${payload.page}&search_query.per_page=${payload.per_page}`;
    const url = `${env.infra_proxy_url}/servers/${payload.server_id}/orgs/${payload.org_id}/data_bags/${payload.name}?${params}`;

    return this.http.get<DataBagSearchResponse>(url, {headers});
  }

  public getDataBagItemDetails(server_id: string, org_id: string, name: string, item_name: string)
  : Observable<DataBagsItemDetails> {
    return this.http.get<DataBagsItemDetails>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/data_bags/${name}/${item_name}`,
      {headers}
    );
  }
}
