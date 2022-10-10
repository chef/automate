import { HttpErrorResponse } from "@angular/common/http";
import { Injectable } from "@angular/core";
import { Actions, ofType, createEffect } from "@ngrx/effects";
import { of as observableof }  from "rxjs";
import { catchError, map, mergeMap } from "rxjs/operators";
import { CreateNotification } from "../notifications/notification.actions";
import { Type } from "../notifications/notification.model";
import {
  GetSsoConfigSuccess,
  GetSsoConfigFailure,
  SsoConfigActionTypes,
} from "./sso-config.actions";
import { SsoConfig } from "./sso-config.model";
import { SsoConfigRequests } from "./sso-config.request";

@Injectable()
export class SsoConfigEffects {
  constructor(private actions$: Actions, private requests: SsoConfigRequests) {}

  getSsoConfig$ = createEffect(() =>
    this.actions$.pipe(
      ofType(SsoConfigActionTypes.GET),
      mergeMap(() =>
        this.requests.getSsoConfig().pipe(
          map((resp: SsoConfig) => 
            new GetSsoConfigSuccess(resp)
            ),
          catchError((error: HttpErrorResponse) =>
            observableof(new GetSsoConfigFailure(error))
          )
        )
      )
    )
  );

  getSsoFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(SsoConfigActionTypes.GET_FAILURE),
      map(({ payload }: GetSsoConfigFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get sso config: ${msg || payload.error}`
        });
      })
    )
  );
}
