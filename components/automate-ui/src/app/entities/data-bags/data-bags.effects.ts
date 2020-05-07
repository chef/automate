import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map } from 'rxjs/operators';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  GetDataBags,
  GetDataBagsSuccess,
  GetDataBagsFailure,
  DataBagsSuccessPayload,
  DataBagsActionTypes
} from './data-bags.action';

import { InfraRoleRequests } from './data-bags.requests';

@Injectable()
export class DataBagsEffects {
  constructor(
    private actions$: Actions,
    private requests: InfraRoleRequests
  ) { }

  @Effect()
  getRoles$ = this.actions$.pipe(
      ofType(DataBagsActionTypes.GET_ALL),
      mergeMap(({ payload: { server_id, org_id } }: GetDataBags) =>
        this.requests.getRoles(server_id, org_id).pipe(
          map((resp: DataBagsSuccessPayload) => new GetDataBagsSuccess(resp)),
          catchError((error: HttpErrorResponse) => observableOf(new GetDataBagsFailure(error))))));

  @Effect()
  getRolesFailure$ = this.actions$.pipe(
      ofType(DataBagsActionTypes.GET_ALL_FAILURE),
      map(({ payload }: GetDataBagsFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get infra roles: ${msg || payload.error}`
        });
      }));

}
