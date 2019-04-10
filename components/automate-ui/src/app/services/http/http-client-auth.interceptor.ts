import { throwError as observableThrowError,  Observable } from 'rxjs';

import { catchError } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { HttpEvent, HttpHandler, HttpInterceptor, HttpRequest } from '@angular/common/http';
import { get } from 'lodash/fp';
import { ChefSessionService } from '../chef-session/chef-session.service';

@Injectable()
export class HttpClientAuthInterceptor implements HttpInterceptor {

  constructor(private chefSession: ChefSessionService) {}

  intercept(request: HttpRequest<any>, next: HttpHandler): Observable<HttpEvent<any>> {
    return next
      .handle(request.clone({
        headers: request.headers.set('Authorization', `Bearer ${this.chefSession.id_token}`)
      })).pipe(
      catchError((response: HttpEvent<any>) => {
        if (get('status', response) === 401) {
          this.chefSession.logout();
        }
        return observableThrowError(response);
      }));
  }
}
