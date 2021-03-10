import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set, pipe } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { RecipeActionTypes, RecipeActions } from './recipe.action';

export interface RecipeEntityState extends EntityState<string> {
  recipesStatus: EntityStatus;
  getAllStatus: EntityStatus;
}

const GET_ALL_STATUS = 'getAllStatus';

export const recipeEntityAdapter: EntityAdapter<string> =
  createEntityAdapter<string>({
   selectId: (recipe: string) => recipe
});

export const RecipeEntityInitialState: RecipeEntityState =
  recipeEntityAdapter.getInitialState(<RecipeEntityState>{
  getAllStatus: EntityStatus.notLoaded
});

export function recipeEntityReducer(
  state: RecipeEntityState = RecipeEntityInitialState,
  action: RecipeActions): RecipeEntityState {

  switch (action.type) {
    case RecipeActionTypes.GET_ALL:
      return set(GET_ALL_STATUS, EntityStatus.loading, recipeEntityAdapter.removeAll(state));

    case RecipeActionTypes.GET_ALL_SUCCESS:
      return pipe(
        set(GET_ALL_STATUS, EntityStatus.loadingSuccess))
        (recipeEntityAdapter.setAll(action.payload.recipes, state)) as
        RecipeEntityState;

    case RecipeActionTypes.GET_ALL_FAILURE:
      return set(GET_ALL_STATUS, EntityStatus.loadingFailure, state);


    default:
      return state;
  }
}

export const getEntityById = (id: string) =>
  (state: RecipeEntityState) => state.entities[id];
