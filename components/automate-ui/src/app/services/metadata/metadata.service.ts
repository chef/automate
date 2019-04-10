import { of as observableOf,  Observable } from 'rxjs';

import { catchError, map } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';

@Injectable()
export class MetadataService {

  constructor(private httpClient: HttpClient) {}

  getBuildVersion(): Observable<string> {
    const url = '/api/v0/version';
    return this.httpClient.get(url).pipe(
      map((res: Object) => res['build_timestamp']),
      catchError(_err => observableOf('')));
  }
}
