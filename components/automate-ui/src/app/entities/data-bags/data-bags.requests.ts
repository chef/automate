import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { DataBagsSuccessPayload } from './data-bags.actions';
import { DataBagDetailsSuccessPayload, DataBagSearchPayload } from './data-bag-details.actions';
import { DataBagsItemDetails } from './data-bags.model';
import { InterceptorSkipHeader } from 'app/services/http/http-client-auth.interceptor';

const headers = new HttpHeaders().set(InterceptorSkipHeader, '');

export interface DataBagSearchResponse {
  nodes: any[];
  total: number;
}


@Injectable()
export class DataBagsRequests {

  constructor(private http: HttpClient) { }

  public getDataBags(server_id: string, org_id: string): Observable<DataBagsSuccessPayload> {
    return this.http.get<DataBagsSuccessPayload>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/data_bags`, {headers});
  }

  public getDataBagDetails(server_id: string, org_id: string, name: string)
  : Observable<DataBagDetailsSuccessPayload> {
    return this.http.get<DataBagDetailsSuccessPayload>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/data_bags?name=${name}`,
      {headers}
    );
  }

  public getDataBagSearchDetails(payload: DataBagSearchPayload)
  : Observable<DataBagSearchResponse> {
    return this.http.get<DataBagSearchResponse>(
      `${env.infra_proxy_url}/servers/${payload.server_id}/orgs/${payload.org_id}/data_bags/${payload.name}?search_query.${payload.query}=id:${payload.databagId}`,
      {headers}
    );
  }

  // ?search_query.q=id:root
  public getDataBagItemDetails(server_id: string, org_id: string, name: string, item_name: string)
  : Observable<DataBagsItemDetails> {
    return this.http.get<DataBagsItemDetails>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/data_bags/${name}/${item_name}`,
      {headers}
    );
  }
}
