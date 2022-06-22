import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, createEffect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map } from 'rxjs/operators';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  RecipeActionTypes,
  GetRecipesSuccess,
  GetRecipes,
  GetRecipesFailure
} from './recipe.action';

import { RecipeRequests } from './recipe.requests';

@Injectable()
export class RecipeEffects {
  constructor(
    private actions$: Actions,
    private requests: RecipeRequests
  ) { }

  getRecipes$ = createEffect(() =>
    this.actions$.pipe(
      ofType(RecipeActionTypes.GET_ALL),
      mergeMap(({ payload: { server_id, org_id, name } }: GetRecipes) =>
        this.requests.getRecipes(server_id, org_id, name).pipe(
          map((resp) => new GetRecipesSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
          observableOf(new GetRecipesFailure(error)))))));

  getRecipesFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(RecipeActionTypes.GET_ALL_FAILURE),
      map(({ payload }: GetRecipesFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get recipes: ${msg || payload.error}`
        });
      })));
}
