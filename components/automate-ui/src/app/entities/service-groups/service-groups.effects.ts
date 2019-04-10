import { mergeMap, map, catchError, withLatestFrom, switchMap } from 'rxjs/operators';
import { of } from 'rxjs';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from '../../ngrx.reducers';
import { Injectable } from '@angular/core';
import { Actions, ofType, Effect } from '@ngrx/effects';
import { HttpErrorResponse } from '@angular/common/http';

import { ServiceGroupsPayload } from './service-groups.model';
import { ServiceGroupEntityState } from './service-groups.reducer';
import {
  ServiceGroupsActionTypes,
  GetServiceGroups,
  GetServiceGroupsSuccess,
  GetServiceGroupsFailure
} from './service-groups.actions';
import { ServiceGroupsRequests } from './service-groups.requests';

@Injectable()
export class ServiceGroupsEffects {
  constructor(
    private actions$: Actions,
    private requests: ServiceGroupsRequests,
    private store: Store<NgrxStateAtom>
  ) {}

  @Effect()
  getServiceGroups$ = this.actions$.pipe(
    ofType(ServiceGroupsActionTypes.GET_SERVICE_GROUPS),
    withLatestFrom(this.store),
    switchMap(([_action, storeState]) => {
      const serviceGroupsState: ServiceGroupEntityState = storeState.serviceGroups;
      return this.requests.fetchServiceGroups(serviceGroupsState.filters).pipe(
        map((payload: ServiceGroupsPayload) => new GetServiceGroupsSuccess(payload)),
        catchError((error: HttpErrorResponse) => of(new GetServiceGroupsFailure(error)))
      );
    }));

  @Effect()
  updateServiceGroupsFilters$ = this.actions$.pipe(
      ofType(ServiceGroupsActionTypes.UPDATE_SERVICE_GROUPS_FILTER),
      mergeMap(() => [
        new GetServiceGroups()
        // new GetServiceGroupsCounts() // When this function is ready, uncomment this line!
      ]));
}
