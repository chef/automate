import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { DataBagsSuccessPayload } from './data-bags.actions';
import { DataBagItemPayload } from './data-bag-details.actions';
import { DataBag, DataBagsItemDetails, DataBagItem } from './data-bags.model';
import { InterceptorSkipHeader } from 'app/services/http/http-client-auth.interceptor';

const headers = new HttpHeaders().set(InterceptorSkipHeader, '');

export interface DataBagSearchResponse {
  items: DataBag[];
  total: number;
}

@Injectable()
export class DataBagsRequests {

  constructor(private http: HttpClient) { }

  public getDataBags(server_id: string, org_id: string): Observable<DataBagsSuccessPayload> {
    return this.http.get<DataBagsSuccessPayload>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/data_bags`, {headers});
  }

  public getDataBagItems(payload: DataBagItemPayload)
  : Observable<DataBagSearchResponse> {

    const wildCardSearch = '*';
    const target = payload.databagName !== '' ?
     'id:' + wildCardSearch + payload.databagName : wildCardSearch + ':';
    const nameTarget = target + wildCardSearch;
    const currentPage = payload.page - 1;
    const params = `search_query.q=${nameTarget}&search_query.page=${currentPage}&search_query.per_page=${payload.per_page}`;
    const url = `${env.infra_proxy_url}/servers/${payload.server_id}/orgs/${payload.org_id}/data_bags/${payload.name}?${params}`;

    return this.http.get<DataBagSearchResponse>(url, {headers});
  }

  public createDataBag(dataBag: DataBag): Observable<{}> {
    return this.http.post(`${env.infra_proxy_url}/servers/${dataBag.server_id}/orgs/${dataBag.org_id}/data_bags`, dataBag);
  }

  public deleteDataBag(server_id: string, org_id: string, name: string): Observable<{}> {
    return this.http.delete(`${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/data_bags/${name}`,
    {headers});
  }

  public getDataBagItemDetails(server_id: string, org_id: string, name: string, item_name: string)
  : Observable<DataBagsItemDetails> {
    return this.http.get<DataBagsItemDetails>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/data_bags/${name}/${item_name}`,
      {headers}
    );
  }

  public createDataBagItem(databagItem: DataBagItem): Observable<{}> {
    return this.http.post(`${env.infra_proxy_url}/servers/${databagItem.server_id}/orgs/${databagItem.org_id}/data_bags/${databagItem.name}`, databagItem);
  }

  public deleteDataBagItem(server_id: string, org_id: string, databag_name: string, name: string):
    Observable<{}> {
      return this.http.delete(`${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/data_bags/${databag_name}/${name}`,
      {headers});
  }

  public updateDataBagItem(dataBagItem: DataBagsItemDetails): Observable<DataBagsItemDetails> {
    return this.http.put<DataBagsItemDetails>(
      `${env.infra_proxy_url}/servers/${dataBagItem.server_id}/orgs/${dataBagItem.org_id}/data_bags/${dataBagItem.data_bag_name}/${dataBagItem.name}`, dataBagItem);
  }
}
