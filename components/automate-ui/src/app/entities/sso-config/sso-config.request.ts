import { HttpClient, HttpHeaders } from "@angular/common/http";
import { Injectable } from "@angular/core";
import { Observable } from "rxjs";

import { environment as env } from "environments/environment";
import { SsoConfig } from "./sso-config.model";
import { InterceptorSkipHeader } from "app/services/http/http-client-auth.interceptor";

export interface SsoConfigResponse {
  ssoConfig: SsoConfig;
}

export interface apiResponse {
  message: string;
}

const headers = new HttpHeaders().set(InterceptorSkipHeader, '');
@Injectable()
export class SsoConfigRequests {
  constructor(private http: HttpClient) { }

  public getSsoConfig(): Observable<SsoConfig> {
    return this.http.get<SsoConfig>(
      `${env.gateway_url}/sso/config`, { headers }
    );
  }

  public createSsoConfig(ssoConfig: SsoConfig): Observable<apiResponse> {
    return this.http.post<apiResponse>(
      `${env.gateway_url}/sso/config`, ssoConfig);
  }

  public deleteSsoConfig(): Observable<apiResponse> {
    return this.http.delete<apiResponse>(`${env.gateway_url}/sso/config`);
  }
}
