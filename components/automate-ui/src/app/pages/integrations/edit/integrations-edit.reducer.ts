import { set } from 'lodash/fp';

import { ManagerActions, ManagerActionTypes } from '../../../entities/managers/manager.actions';

export enum Status {
  notUpdated = 'notUpdated',
  updating = 'updating',
  success = 'success',
  failure = 'failure'
}

export interface IntegrationsEditState {
  status: Status;
}

export const IntegrationsEditInitialState: IntegrationsEditState = {
  status: Status.notUpdated
};

export function integrationsEditReducer(state: IntegrationsEditState = IntegrationsEditInitialState,
                                        action: ManagerActions) {
  switch (action.type) {
    case ManagerActionTypes.UPDATE:
      return set('status', Status.updating, state);

    case ManagerActionTypes.UPDATE_SUCCESS:
      return set('status', Status.success, state);

    case ManagerActionTypes.NAV_EDIT:
      return set('status', Status.notUpdated, state);

    default:
      return state;
  }
}
