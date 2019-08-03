import { debounceTime, mergeMap, map, catchError } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { interval, of } from 'rxjs';

import { ProjectConstants } from 'app/entities/projects/project.model';
import {
  UserPermsTypes,
  GetAllUserPermsSuccess,
  GetAllUserPermsFailure,
  GetSomeUserPermsSuccess,
  GetSomeUserPermsFailure,
  GetSomeUserPerms,
  UserPermsResponsePayload,
  GetUserParamPerms,
  GetUserParamPermsSuccess,
  GetUserParamPermsFailure
} from './userperms.actions';
import { UserPermsRequests } from './userperms.requests';

const POLLING_INTERVAL_IN_SECONDS = 300; // every 5 minutes

@Injectable()
export class UserPermEffects {
  constructor(
    private actions$: Actions,
    private requests: UserPermsRequests
  ) { }

  // include debounce here because each request of this type is a true duplicate
  @Effect()
  fetchAllPerms$ = this.actions$.pipe(
    ofType(UserPermsTypes.GET_ALL),
    debounceTime(20),
    mergeMap(() => this.requests.fetchAll().pipe(
      map((resp: UserPermsResponsePayload) => new GetAllUserPermsSuccess(resp.endpoints)),
      catchError((error: HttpErrorResponse) => of(new GetAllUserPermsFailure(error))))));

  @Effect()
  fetchSomePerms$ = this.actions$.pipe(
    ofType(UserPermsTypes.GET_SOME),
    mergeMap((action: GetSomeUserPerms) => this.requests.fetchSome(action.payload).pipe(
      map((resp: UserPermsResponsePayload) => new GetSomeUserPermsSuccess(resp.endpoints)),
      catchError((error: HttpErrorResponse) => of(new GetSomeUserPermsFailure(error))))));

  @Effect()
  fetchParameterizedPerms$ = this.actions$.pipe(
    ofType(UserPermsTypes.GET_PARAMETERIZED),
    mergeMap((action: GetUserParamPerms) => this.requests.fetchParameterized(action.payload).pipe(
      map((resp: UserPermsResponsePayload) => new GetUserParamPermsSuccess(resp.endpoints)),
      catchError((error: HttpErrorResponse) => of(new GetUserParamPermsFailure(error))))));

  @Effect()
  fetchApplyRulesStatusAllowed$ = interval(1000 * POLLING_INTERVAL_IN_SECONDS).pipe(
    mergeMap(() => this.requests.fetchSome({ paths: [ProjectConstants.APPLY_RULES_ENDPOINT] }).pipe(
      map((resp: UserPermsResponsePayload) => new GetSomeUserPermsSuccess(resp.endpoints)),
      catchError((error: HttpErrorResponse) => of(new GetSomeUserPermsFailure(error))))));
}
