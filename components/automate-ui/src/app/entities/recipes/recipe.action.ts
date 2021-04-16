import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';

export enum RecipeActionTypes {
  GET_ALL          = 'RECIPES::GET_ALL',
  GET_ALL_SUCCESS  = 'RECIPES::GET_ALL::SUCCESS',
  GET_ALL_FAILURE  = 'RECIPES::GET_ALL::FAILURE'
}

export interface RecipesSuccessPayload {
  recipes: string[];
}

export class GetRecipes implements Action {
  readonly type = RecipeActionTypes.GET_ALL;
  constructor(public payload: { server_id: string, org_id: string, name: string }) { }
}

export class GetRecipesSuccess implements Action {
  readonly type = RecipeActionTypes.GET_ALL_SUCCESS;
  constructor(public payload) { }
}

export class GetRecipesFailure implements Action {
  readonly type = RecipeActionTypes.GET_ALL_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export type RecipeActions =
  | GetRecipes
  | GetRecipesSuccess
  | GetRecipesFailure;
