import { set } from 'lodash/fp';
import { ROUTER_NAVIGATION, RouterAction } from '@ngrx/router-store';

import { ManagerActions, ManagerActionTypes } from '../../../entities/managers/manager.actions';

export enum Status {
  notCreated = 'notCreated',
  saving = 'saving',
  success = 'success',
  failure = 'failure'
}
export interface IntegrationsAddState {
  status: Status;
}

export const IntegrationsAddInitialState: IntegrationsAddState = {
  status: Status.notCreated
};

export function integrationsAddReducer(state: IntegrationsAddState = IntegrationsAddInitialState,
                                       action: ManagerActions | RouterAction<any>) {
  switch (action.type) {
    case ManagerActionTypes.CREATE:
      return set('status', Status.saving, state);

    case ManagerActionTypes.CREATE_SUCCESS:
      return set('status', Status.success, state);

    case ManagerActionTypes.CREATE_FAILURE:
      return set('status', Status.failure, state);

    case ROUTER_NAVIGATION:
      return set('status', Status.notCreated, state);

    default:
      return state;
  }
}
