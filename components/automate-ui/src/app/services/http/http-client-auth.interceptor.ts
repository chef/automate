import {
  HttpEvent,
  HttpErrorResponse,
  HttpHandler,
  HttpInterceptor,
  HttpRequest
} from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Store } from '@ngrx/store';
import { throwError as observableThrowError, Observable } from 'rxjs';
import { catchError, mergeMap, take } from 'rxjs/operators';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { ChefSessionService } from 'app/services/chef-session/chef-session.service';
import * as selectors from 'app/services/projects-filter/projects-filter.selectors';
import { ProjectsFilterOption } from '../projects-filter/projects-filter.reducer';

export const InterceptorSkipHeader = 'Skip-Interceptor';

@Injectable()
export class HttpClientAuthInterceptor implements HttpInterceptor {

  private projects: string;

  constructor(
    private chefSession: ChefSessionService,
    private store: Store<NgrxStateAtom>
  ) {
    this.store.select(selectors.options)
      .subscribe((options: ProjectsFilterOption[]) => {
        this.projects = options.filter(p => p.checked).map(p => p.value).join(', ');
      });
  }

  intercept(request: HttpRequest<any>, next: HttpHandler): Observable<HttpEvent<any>> {
    let headers = request.headers;

    // TODO(sr): Sadly, our UI code depends on the API ignoring unknown fields in the
    //           request payloads in many places. We should not send any of those. But
    //           Fixing that blows the scope of my little API validation adventure, so
    //           we take a shortcut: ask the API not to be too strict on us.
    headers = headers.set('Content-Type', 'application/json+lax');
    // Check and then remove `unfiltered` param; it is a piggybacked parameter
    // needed by this interceptor, not to be passed on.
    // It allows certain URLs to suppress sending the projects filter.
    const filtered = request.params.get('unfiltered') !== 'true';
    const params = request.params.delete('unfiltered');

    if (this.projects && filtered) {
      headers = headers.set('projects', this.projects);
    }

    return this.chefSession.token_provider.pipe(
      take(1),
      mergeMap(id_token => {
        // TODO(sr): Can we check if the request is sent to some external API?
        //           Right now, we're throwing the ID token across the internet for telemetry,
        //           where it's not needed. It would be nice to _not_ do that.
        headers = headers.set('Authorization', `Bearer ${id_token}`);
        // Check the interceptor skip header in API request
        // To avoid session logout if 401 raised from external API
        if (request.headers.has(InterceptorSkipHeader)) {
          headers = headers.delete(InterceptorSkipHeader);
          return next.handle(request.clone({ headers }));
        }
        return next
          .handle(request.clone({ headers, params })).pipe(
            catchError((error: HttpErrorResponse) => {
              if (error.status === 401) {
                this.chefSession.logout();
              }
              return observableThrowError(error);
            }));
      })
    );
  }
}
