import { HttpErrorResponse } from "@angular/common/http";
import { Injectable } from "@angular/core";
import { Actions, ofType, createEffect } from "@ngrx/effects";
import { HttpStatus } from 'app/types/types';
import { of as observableof } from "rxjs";
import { catchError, filter, map, mergeMap } from "rxjs/operators";
import { CreateNotification } from "../notifications/notification.actions";
import { Type } from "../notifications/notification.model";
import {
  GetSsoConfigSuccess,
  GetSsoConfigFailure,
  SsoConfigActionTypes,
  CreateSsoConfig,
  CreateSsoConfigSuccess,
  CreateSsoConfigFailure,
  DeleteSsoConfigSuccess,
  DeleteSsoConfigFailure,
} from "./sso-config.actions";
import { SsoConfig } from "./sso-config.model";
import { SsoConfigRequests } from "./sso-config.request";

@Injectable()
export class SsoConfigEffects {
  constructor(private actions$: Actions, private requests: SsoConfigRequests) { }

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

  getSsoConfigFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(SsoConfigActionTypes.GET_FAILURE),
      map(() => {
        return new CreateNotification({
          type: Type.error,
          message: `Could not get SSO config`
        });
      })
    )
  );


  createSsoConfig$ = createEffect(() =>
    this.actions$.pipe(
      ofType(SsoConfigActionTypes.CREATE),
      mergeMap(({ payload }: CreateSsoConfig) =>
        this.requests.createSsoConfig(payload).pipe(
          map((resp) => new CreateSsoConfigSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
            observableof(new CreateSsoConfigFailure(error))
          )
        )
      )
    ))

  createSsoConfigSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(SsoConfigActionTypes.CREATE_SUCCESS),
      map(() =>
        new CreateNotification({
          type: Type.info,
          message: `Successfully patched SSO config`
        })
      ))
  );

  createSsoConfigFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(SsoConfigActionTypes.CREATE_FAILURE),
      filter(
        ({ payload }: CreateSsoConfigFailure) =>
          payload.status !== HttpStatus.CONFLICT
      ),
      map(() =>
        new CreateNotification({
          type: Type.error,
          message: `Could not patch SSO config`
        })
      )
    ));


  deleteSsoConfig$ = createEffect(() =>
    this.actions$.pipe(
      ofType(SsoConfigActionTypes.DELETE),
      mergeMap(() =>
        this.requests.deleteSsoConfig().pipe(
          map(resp => new DeleteSsoConfigSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
            observableof(new DeleteSsoConfigFailure(error))
          )
        )
      )
    ));

  deleteSsoConfigSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(SsoConfigActionTypes.DELETE_SUCCESS),
      map(() => {
        return new CreateNotification({
          type: Type.info,
          message: `Successfully removed SSO config`
        });
      })
    ));

  deleteSsoConfigFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(SsoConfigActionTypes.DELETE_FAILURE),
      map(() => {
        return new CreateNotification({
          type: Type.error,
          message: `Could not remove SSO config`
        });
      })
    )
  );
}
