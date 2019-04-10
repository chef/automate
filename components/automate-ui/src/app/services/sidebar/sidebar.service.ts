import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';

import { environment } from '../../../environments/environment';
const CONFIG_MGMT_URL = environment.config_mgmt_url;

@Injectable()
export class SidebarService {
  constructor(
    private httpClient: HttpClient
  ) {}

  getOrgs(): Observable<Array<string>> {
    const url = `${CONFIG_MGMT_URL}/organizations`;

    return this.httpClient.get<string[]>(url);
  }

  getChefServers(): Observable<Array<string>> {
    const url = `${CONFIG_MGMT_URL}/source_fqdns`;

    return this.httpClient.get<string[]>(url);
  }
}
