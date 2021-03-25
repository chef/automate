import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Store } from '@ngrx/store';
import { Actions, createEffect, ofType } from '@ngrx/effects';
import {
  throttleTime,
  mergeMap,
  map,
  catchError,
  withLatestFrom,
  switchMap
} from 'rxjs/operators';
import { of } from 'rxjs';
import * as moment from 'moment/moment';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { IndexedEntities } from 'app/entities/entities';
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
import { getlastFetchTime, allPerms } from './userperms.selectors';
import { UserPermEntity } from './userperms.entity';

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

  fetchAllPerms$ = createEffect(() =>
    this.actions$.pipe(
    ofType(UserPermsTypes.GET_ALL),
    throttleTime(this.freshLimitInMilliseconds),
    switchMap(() => {
      return this.requests.fetchAll().pipe(
        map((resp: UserPermsResponsePayload) => new GetAllUserPermsSuccess(resp.endpoints)),
        catchError((error: HttpErrorResponse) => of(new GetAllUserPermsFailure(error))));
    })));

  fetchSomePerms$ = createEffect(() =>
    this.actions$.pipe(
    ofType(UserPermsTypes.GET_SOME),
    withLatestFrom(this.store.select(getlastFetchTime)),
    withLatestFrom(this.store.select(allPerms)),
    switchMap(
      ([[action, lastTime], list]: [[GetSomeUserPerms, Date], IndexedEntities<UserPermEntity>]) => {
        const isFresh = !!list && !this.stale(lastTime);
        return isFresh ?
          // TODO: This works, but it has extraneous 'id' property in the payload.
          // Should strip that from each entry in the list.
          of(new GetSomeUserPermsSuccess(list))

          : this.requests.fetchSome(action.payload).pipe(
            map((resp: UserPermsResponsePayload) => new GetSomeUserPermsSuccess(resp.endpoints)),
            catchError((error: HttpErrorResponse) => of(new GetSomeUserPermsFailure(error))));
      })));

  fetchParameterizedPerms$ = createEffect(() =>
    this.actions$.pipe(
    ofType(UserPermsTypes.GET_PARAMETERIZED),
    mergeMap((action: GetUserParamPerms) => this.requests.fetchParameterized(action.payload).pipe(
      map((resp: UserPermsResponsePayload) => new GetUserParamPermsSuccess(resp.endpoints)),
      catchError((error: HttpErrorResponse) => of(new GetUserParamPermsFailure(error)))))));

  private stale(lastTime: Date): boolean {
    return moment().diff(moment(lastTime)) > this.freshLimitPlusANudge;
  }
}
