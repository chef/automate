import { HttpClient, HttpHeaders } from "@angular/common/http";
import { Injectable } from "@angular/core";
import { Observable } from "rxjs";

import { environment as env } from "environments/environment";
import { SsoConfig } from "./sso-config.model";
import { InterceptorSkipHeader } from "app/services/http/http-client-auth.interceptor";

export interface SsoConfigResponse {
  ssoConfig: SsoConfig;
}

export interface ApiResponse {
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

  public createSsoConfig(ssoConfig: SsoConfig): Observable<ApiResponse> {
    return this.http.post<ApiResponse>(
      `${env.gateway_url}/sso/config`, ssoConfig);
  }

  public deleteSsoConfig(): Observable<ApiResponse> {
    return this.http.delete<ApiResponse>(`${env.gateway_url}/sso/config`);
  }
}
