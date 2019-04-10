import { of as observableOf,  Observable } from 'rxjs';

import { catchError, map } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { HttpClient, HttpResponse } from '@angular/common/http';

import { environment } from '../../../environments/environment';
const GATEWAY_URL = environment.gateway_url;

export interface Config {
  telemetryEnabled: boolean;
  telemetryUrl: string;
  licenseId: string;
  licenseType: string;
  customerId: string;
  customerName: string;
  maxNodes: number;
  deploymentId: string;
}

@Injectable()
export class ConfigService {
  public defaultLicenseId = '00000000-0000-0000-0000-111111111111';
  public defaultDeployId = '00000000-0000-0000-0000-222222222222';

  // Default to turning telemetry off if we run into errors since
  // we won't have useful license information anyways.
  // This will be the case for dev mode when the license-control-service is not running
  private defaults: any = {
    telemetry_enabled: false,
    telemetry_url: 'https://telemetry-acceptance.chef.io',
    license_id: this.defaultLicenseId,
    license_type: 'Unavailable',
    customer_id: '',
    customer_name: '',
    max_nodes: 0,
    deployment_id: this.defaultLicenseId
  };

  constructor(private httpClient: HttpClient) { }

  getConfig(): Observable<Config> {
    return this.httpClient.get<Config>(`${GATEWAY_URL}/telemetry/config`,
                                       { observe: 'response' }).pipe(
      map((res: HttpResponse<Config>) => {
        if (res.status === 404) {
          return this.defaults;
        } else {
          return res.body;
        }
      }),
      catchError((_err) => observableOf(this.defaults)),
      map((configJson: any) => ({
        telemetryEnabled: configJson.telemetry_enabled,
        telemetryUrl: configJson.telemetry_url,
        licenseId: configJson.license_id,
        licenseType: configJson.license_type,
        customerId: configJson.customer_id,
        customerName: configJson.customer_name,
        maxNodes: configJson.max_nodes,
        deploymentId: configJson.deployment_id
      })));
  }
}
