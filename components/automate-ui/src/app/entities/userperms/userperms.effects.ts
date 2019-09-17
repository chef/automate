import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Store } from '@ngrx/store';
import { Actions, Effect, ofType } from '@ngrx/effects';
import {
  throttleTime,
  mergeMap,
  map,
  catchError,
  withLatestFrom,
  switchMap,
  filter
} from 'rxjs/operators';
import { of } from 'rxjs';
import * as moment from 'moment';

import { NgrxStateAtom } from 'app/ngrx.reducers';
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
import { getlastFetchTime } from './userperms.selectors';

@Injectable()
export class UserPermEffects {
  constructor(
    private actions$: Actions,
    private requests: UserPermsRequests,
    private store: Store<NgrxStateAtom>
  ) { }

  // the target limit
  private freshLimitInMilliseconds = 30000;

  // to account for a flurry of IntrospectSome calls almost concurrent
  // with our IntrospectAll call--this will happen frequently!--just
  // extend the fresh limit by a nudge for the IntrospectSome calls
  private freshLimitPlusANudge = 33000;

  @Effect()
  fetchAllPerms$ = this.actions$.pipe(
    ofType(UserPermsTypes.GET_ALL),
    throttleTime(this.freshLimitInMilliseconds),
    switchMap(() => {
      return this.requests.fetchAll().pipe(
        map((resp: UserPermsResponsePayload) => new GetAllUserPermsSuccess(resp.endpoints)),
        catchError((error: HttpErrorResponse) => of(new GetAllUserPermsFailure(error))));
    }));

  @Effect()
  fetchSomePerms$ = this.actions$.pipe(
    ofType(UserPermsTypes.GET_SOME),
    withLatestFrom(this.store.select(getlastFetchTime)),
    filter(([_action, lastTime]) => this.stale(lastTime)),
    mergeMap(
      ([action, _]: [GetSomeUserPerms, Date]) => this.requests.fetchSome(action.payload).pipe(
      map((resp: UserPermsResponsePayload) => new GetSomeUserPermsSuccess(resp.endpoints)),
      catchError((error: HttpErrorResponse) => of(new GetSomeUserPermsFailure(error))))));

  @Effect()
  fetchParameterizedPerms$ = this.actions$.pipe(
    ofType(UserPermsTypes.GET_PARAMETERIZED),
    mergeMap((action: GetUserParamPerms) => this.requests.fetchParameterized(action.payload).pipe(
      map((resp: UserPermsResponsePayload) => new GetUserParamPermsSuccess(resp.endpoints)),
      catchError((error: HttpErrorResponse) => of(new GetUserParamPermsFailure(error))))));

  private stale(lastTime: Date): boolean {
    return moment().diff(moment(lastTime)) > this.freshLimitPlusANudge;
  }
}
