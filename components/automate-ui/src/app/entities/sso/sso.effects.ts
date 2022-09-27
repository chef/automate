import { HttpErrorResponse } from "@angular/common/http";
import { Injectable } from "@angular/core";
import { Actions, ofType } from "@ngrx/effects";
import { createEffect } from "@ngrx/effects";
import { of } from "rxjs";
import { catchError, map, mergeMap } from "rxjs/operators";
import { GetAllSsoConfig, GetAllSsoConfigFailure, GetAllSsoConfigSuccess, SsoActionTypes } from "./sso.actions";
import { SsoRequests } from "./sso.request";

@Injectable()
export class SsoEffects {
    constructor(
        private actions$: Actions,
        private requests: SsoRequests
    ){ }

    getsso$ = createEffect(() =>
    this.actions$.pipe(ofType<GetAllSsoConfig>(SsoActionTypes.GET_ALL),
    mergeMap((_action) =>
      this.requests.getAll().pipe(
        map(resp => new GetAllSsoConfigSuccess(resp)),
        catchError((error: HttpErrorResponse) => of(new GetAllSsoConfigFailure(error))))
    )));
}