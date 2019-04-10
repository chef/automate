import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';

import { environment as env } from '../../../environments/environment';
import {
  LicenseStatus,
  ApplyLicensePayload,
  ApplyLicenseResponse,
  RequestLicensePayload,
  RequestLicenseResponse
} from './license.model';

@Injectable()
export class LicenseStatusRequests {

  constructor(private http: HttpClient) {}

  public fetchLicenseStatus(): Observable<LicenseStatus> {
    return this.http.get<LicenseStatus>(`${env.gateway_url}/license/status`);
  }

  public applyLicense(licensePayload: ApplyLicensePayload): Observable<ApplyLicenseResponse> {
    return this.http.post<ApplyLicenseResponse>(`${env.gateway_url}/license/apply`, licensePayload);
  }

  public requestLicense(requestPayload: RequestLicensePayload):
  Observable<RequestLicenseResponse> {
    return this.http.post<RequestLicenseResponse>(`${env.gateway_url}/license/request`,
    requestPayload);
  }
}
