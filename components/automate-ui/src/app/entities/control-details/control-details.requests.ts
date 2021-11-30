import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { ControlDetail } from './control-details.model';

export interface ControlDetailResponse {
  controlDetail: ControlDetail[];
}

@Injectable()
export class ControlDetailRequests {
  constructor(private http: HttpClient) { }

  public GetControlDetail(payload: any ) : Observable<ControlDetailResponse> {
    const filters = {'filters': payload.filters};
    const url = `${env.compliance_url}/reporting/reports/id/${payload.report_id}`;
    return this.http.post<ControlDetailResponse>(url, filters);
  }
}
