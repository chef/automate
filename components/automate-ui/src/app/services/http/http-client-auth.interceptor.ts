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
import { catchError } from 'rxjs/operators';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { ChefSessionService } from 'app/services/chef-session/chef-session.service';
import * as selectors from 'app/services/projects-filter/projects-filter.selectors';
import { ProjectsFilterOption } from '../projects-filter/projects-filter.reducer';

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
    let headers = request.headers.set('Authorization', `Bearer ${this.chefSession.id_token}`);
    const filtered = request.params.get('unfiltered') !== 'true';
    // Uncomment here and after to clone() arg list
    // after we've upgraded to angular 7.2+ (for this issue:
    // https://github.com/angular/angular/issues/18812).
    // const params = request.params.delete('unfiltered');
    if (this.projects && filtered) {
      headers = headers.set('projects', this.projects);
    }
    return next
      .handle(request.clone({ headers })).pipe(
        catchError((error: HttpErrorResponse) => {
          if (error.status === 401) {
            // TODO(sr) When refactoring the session handling in automate-ui and ngrx,
            // this should be dealt with: right now, if any API request gets a 401,
            // the user will be signed out. While we attempt to save the user from
            // having to choose their login method again (that's the `true`), we don't
            // know on which UI page this has occourred (that's why we pass "/").
            this.chefSession.logout('/', true);
          }
          return observableThrowError(error);
        }));
  }
}
