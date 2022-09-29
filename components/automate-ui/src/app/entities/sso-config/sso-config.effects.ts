import { HttpErrorResponse } from "@angular/common/http";
import { Injectable } from "@angular/core";
import { Actions, ofType, createEffect } from "@ngrx/effects";
import { of as observableof }  from "rxjs";
import { catchError, map, mergeMap } from "rxjs/operators";
import { CreateNotification } from "../notifications/notification.actions";
import { Type } from "../notifications/notification.model";
import {
  GetAllSsoConfigFailure,
  GetAllSsoConfigSuccess,
  SsoActionTypes,
} from "./sso-config.actions";
import { SsoConfigRequests } from "./sso-config.request";

@Injectable()
export class SsoConfigEffects {
  constructor(private actions$: Actions, private requests: SsoConfigRequests) {}

  getAllSso$ = createEffect(() =>
    this.actions$.pipe(
      ofType(SsoActionTypes.GET_ALL),
      mergeMap(() =>
        this.requests.getAllConfig().pipe(
          map((resp) => 
            new GetAllSsoConfigSuccess(resp)
            ),
          catchError((error: HttpErrorResponse) =>
            observableof(new GetAllSsoConfigFailure(error))
          )
        )
      )
    )
  );

  getAllSsoFailure = createEffect(() =>
    this.actions$.pipe(
      ofType<GetAllSsoConfigFailure>(SsoActionTypes.GET_ALL_FAILURE),
      map(
        ({ payload: { error } }) =>
          new CreateNotification({
            type: Type.error,
            message: `Could not get sso: ${error.error || error}.`,
          })
      )
    )
  );
}


