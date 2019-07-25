import { map } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment } from 'environments/environment';
import { StatsService } from './stats.service';

const CC_API_URL = environment.compliance_url;

@Injectable()
export class SuggestionsService {
  constructor(
    private httpClient: HttpClient,
    private statsService: StatsService
  ) {}

  getSuggestions(type: string, text: string, filters: any): Observable<any> {
    const url = `${CC_API_URL}/reporting/suggestions`;
    const formatted = this.statsService.formatFilters(filters);
    const body = {type, text, filters: formatted};
    return this.httpClient.post<any>(url, body).pipe(
      map(({suggestions}) => suggestions.slice(0, 100) || []));
  }
}
