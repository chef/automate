import { HttpClient, HttpHeaders } from "@angular/common/http";
import { Injectable } from "@angular/core";
import { Observable } from "rxjs";

import { environment as env } from "environments/environment";
import { SsoConfig } from "./sso-config.model";
import { InterceptorSkipHeader } from "app/services/http/http-client-auth.interceptor";

export interface SsoConfigResponse {
  ssoConfig: SsoConfig;
}

const headers = new HttpHeaders().set(InterceptorSkipHeader, '');
@Injectable()
export class SsoConfigRequests {
  constructor(private http: HttpClient) {}

  public getSsoConfig(): Observable<SsoConfig> {
    return this.http.get<SsoConfig>(
      `${env.gateway_url}/sso/config`, { headers }
    );
  }
}
