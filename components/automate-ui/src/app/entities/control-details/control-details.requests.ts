import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { ControlDetail } from './control-details.model';
// import { InterceptorSkipHeader } from 'app/services/http/http-client-auth.interceptor';

// const headers = new HttpHeaders().set(InterceptorSkipHeader, '');

export interface ControlDetailsResponse {
  controlDetails: ControlDetail[];
}

@Injectable()
export class ControlDetailsRequests {

  constructor(private http: HttpClient) { }

  public GetControlDetails(filters: any )   
  : Observable<ControlDetailsResponse> {
    const url = `${env.compliance_url}/reporting/reports/id/${filters.reportID}`;
    return this.http.post<ControlDetailsResponse>(url, filters.filters);
  }

}
