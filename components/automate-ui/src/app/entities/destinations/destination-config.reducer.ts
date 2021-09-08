import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { DestinationActionTypes, DestinationActions } from './destination.actions';
import {  GlobalConfig } from './destination.model';


export interface GlobalConfigEntityState extends EntityState<GlobalConfig> {
  globalConfigStatus: EntityStatus;
  globalConfig: GlobalConfig;
}


const GLOBAL_CONFIG_STATUS = 'globalConfigStatus';
const GLOBAL_CONFIG = 'globalConfig';


export const globalConfigEntityAdapter: EntityAdapter<GlobalConfig> =
  createEntityAdapter<GlobalConfig>();


export const GlobalConfigEntityInitialState: GlobalConfigEntityState =
globalConfigEntityAdapter.getInitialState({
    status: EntityStatus.notLoaded,
    saveStatus: EntityStatus.notLoaded,
    saveError: null,
    globalConfigStatus: EntityStatus.notLoaded,
    globalConfig: null
  });

export function globalConfigEntityReducer(
  state: GlobalConfigEntityState = GlobalConfigEntityInitialState,
  action: DestinationActions): GlobalConfigEntityState {
    switch (action.type) {

        case DestinationActionTypes.GLOBAL_CONFIG: {
          return set(
            GLOBAL_CONFIG_STATUS,
            EntityStatus.loading,
            state
          ) as GlobalConfigEntityState;
        }
        case DestinationActionTypes.GLOBAL_CONFIG_SUCCESS: {
          const configStatusState = set(
            GLOBAL_CONFIG_STATUS,
            EntityStatus.loadingSuccess,
            state
            );
            return set(
              GLOBAL_CONFIG,
              action.payload,
              configStatusState
            ) as GlobalConfigEntityState;
        }
        case DestinationActionTypes.GLOBAL_CONFIG_FAILURE: {
          return set(GLOBAL_CONFIG_STATUS, EntityStatus.loadingFailure, state
            ) as GlobalConfigEntityState;
        }
      }
    return state;
}

