import { pipe, set } from 'lodash/fp';

import { Manager } from '../../../entities/managers/manager.model';
import { ManagerActions, ManagerActionTypes } from '../../../entities/managers/manager.actions';

export enum Status {
  notLoaded = 'notLoaded',
  loading = 'loading',
  success = 'success',
  failure = 'failure'
}

export interface IntegrationsDetailState {
  manager: Manager;
  managerLoadingStatus: Status;
  managerNodes: any[];
  managerNodesTotal: number;
  managerNodesPage: number;
  managerNodesPerPage: number;
  managerNodesLoadingStatus: Status;
}

export const IntegrationsDetailInitialState: IntegrationsDetailState = {
  manager: null,
  managerLoadingStatus: Status.notLoaded,
  managerNodes: [],
  managerNodesTotal: 0,
  managerNodesPage: 1,
  managerNodesPerPage: 100,
  managerNodesLoadingStatus: Status.notLoaded
};

export function integrationsDetailReducer(
  state: IntegrationsDetailState = IntegrationsDetailInitialState,
  action: ManagerActions) {

  switch (action.type) {
    case ManagerActionTypes.GET:
      return set('managerLoadingStatus', Status.loading, state);

    case ManagerActionTypes.GET_SUCCESS:
      const { manager } = action.payload;
      return pipe(
        set('manager', manager),
        set('managerLoadingStatus', Status.success)
      )(state);

    case ManagerActionTypes.GET_NODES:
      return set('managerNodesLoadingStatus', Status.loading, state);

    case ManagerActionTypes.GET_NODES_SUCCESS:
      const { nodes, total } = action.payload;
      return pipe(
        set('managerNodes', nodes),
        set('managerNodesTotal', total),
        set('managerNodesLoadingStatus', Status.success)
      )(state);

    case ManagerActionTypes.NAV_DETAIL:
      const { page, per_page } = action.payload;
      return pipe(
        set('managerLoadingStatus', Status.notLoaded),
        set('managerNodesPage', page),
        set('managerNodesPerPage', per_page),
        set('managerNodesLoadingStatus', Status.notLoaded)
      )(state);

    default:
      return state;
  }
}
