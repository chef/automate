import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { ControlDetail } from './control-details.model';

export interface ControlDetailsResponse {
  controlDetails: ControlDetail[];
}

@Injectable()
export class ControlDetailsRequests {
  constructor(private http: HttpClient) { }

  public GetControlDetails(payload: any ) : Observable<ControlDetailsResponse> {
    const filters = {'filters': payload.filters};
    const url = `${env.compliance_url}/reporting/reports/id/${payload.report_id}`;
    return this.http.post<ControlDetailsResponse>(url, filters);
  }
}
