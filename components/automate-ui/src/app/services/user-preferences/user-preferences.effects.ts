import { mergeMap, map, catchError } from 'rxjs/operators';
import { of } from 'rxjs';
import { Injectable } from '@angular/core';
import { Actions, ofType, createEffect } from '@ngrx/effects';
import { HttpErrorResponse } from '@angular/common/http';

import { UserPreferencesRequests } from './user-preferences.requests';

import {
  UserPreferenceResponse
} from './user-preferences.model';

import {
  UserPreferencesActionTypes,
  GetUserPreferencesSuccess,
  GetUserPreferencesFailure,
  UpdateUserPreferences,
  UpdateUserPreferencesSuccess,
  UpdateUserPreferencesFailure
} from './user-preferences.actions';

import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

@Injectable()
export class UserPreferencesEffects {
  constructor(
    private actions$: Actions,
    private requests: UserPreferencesRequests
  ) {}

  getUserPreferences$ = createEffect(() => {
    return this.actions$.pipe(
      ofType(UserPreferencesActionTypes.GET),
      mergeMap(() =>
          this.requests.fetchUserPreferences().pipe(
          map((response: any) => {
            const userPref: UserPreferenceResponse = {
              user: response.user,
              time_format: {
                value: response.settings.date_format.value,
                valid_values: response.settings.date_format.valid_values
              }
            };
            return new GetUserPreferencesSuccess(userPref);
          }),
          catchError((error: HttpErrorResponse) => of(new GetUserPreferencesFailure(error))
          ))
      ));
    }
  );

  updateUserPreferences$ = createEffect(() =>
    this.actions$.pipe(
      ofType(UserPreferencesActionTypes.UPDATE),
      mergeMap((action: UpdateUserPreferences) =>
        this.requests.updateUserPreferences(action.payload).pipe(
          map((response: any) =>
            new UpdateUserPreferencesSuccess(response)),
          catchError((error: HttpErrorResponse) => of(new UpdateUserPreferencesFailure(error))
          ))
      ))
  );

  updateUserPreferencesSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(UserPreferencesActionTypes.UPDATE_SUCCESS),
      mergeMap(() => {
        const msg = 'Updated user preferences.';
        return [
          new CreateNotification({
            type: Type.info,
            message: msg
          })
        ];
      }))
  );

  updateUserPreferencesFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(UserPreferencesActionTypes.UPDATE_FAILURE),
      map((action: UpdateUserPreferencesFailure) => {
        const msg = `Could not update user preferences: ${action.payload.error}`;
        return new CreateNotification({
          type: Type.error,
          message: msg
        });
      })
    ));

  getUserPreferencesFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(UserPreferencesActionTypes.GET_FAILURE),
    map((action: GetUserPreferencesFailure) => {
      const msg = `Could not get user preferences: ${action.payload.error}`;
      return new CreateNotification({
        type: Type.error,
        message: msg
      });
    })
  ));
}
