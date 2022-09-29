import { HttpClient, HttpHeaders } from "@angular/common/http";
import { Injectable } from "@angular/core";
import { Observable } from "rxjs";

import { environment as env } from "environments/environment";
import { SsoConfig } from "./sso-config.model";
import { InterceptorSkipHeader } from "app/services/http/http-client-auth.interceptor";
import { GetAllConfigResponse } from "./sso-config.actions";

export interface SsoPayloadResponse {
  sso: SsoConfig;
}

const headers = new HttpHeaders().set(InterceptorSkipHeader, '');
@Injectable()
export class SsoConfigRequests {
  constructor(private http: HttpClient) {}

  public getAllConfig(): Observable<GetAllConfigResponse> {
    const abc =  this.http.get<GetAllConfigResponse>(
      `${env.gateway_url}/sso/config`,
      { headers }
    );
    console.log(abc);
    return abc;
  }
}
