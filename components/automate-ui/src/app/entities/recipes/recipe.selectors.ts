import { createSelector, createFeatureSelector } from '@ngrx/store';
import { RecipeEntityState, recipeEntityAdapter } from './recipe.reducer';

export const recipeState = createFeatureSelector<RecipeEntityState>('recipes');
export const {
  selectAll: allRecipes,
  selectEntities: recipeEntities
} = recipeEntityAdapter.getSelectors(recipeState);

export const recipeStatus = createSelector(
  recipeState,
  (state) => state.recipesStatus
);

export const getAllStatus = createSelector(
  recipeState,
  (state) => state.getAllStatus
);
