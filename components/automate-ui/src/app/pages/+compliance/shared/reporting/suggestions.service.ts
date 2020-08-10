import { map } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment } from 'environments/environment';
import { StatsService } from './stats.service';
import { ReportQuery } from './report-query.service';

const CC_API_URL = environment.compliance_url;

@Injectable()
export class SuggestionsService {
  constructor(
    private httpClient: HttpClient,
    private statsService: StatsService
  ) {}

  selectedControlTagKey: string;

  getSuggestions(type: string, text: string, reportQuery: ReportQuery): Observable<any> {
    const url = `${CC_API_URL}/reporting/suggestions`;
    const formatted = this.statsService.formatFilters(reportQuery);
        const body = {
          type,
          text,
          type_key: this.selectedControlTagKey,
          filters: formatted
        };
    return this.httpClient.post<any>(url, body).pipe(
      map(({suggestions}) => suggestions.slice(0, 100) || []));
  }
}
