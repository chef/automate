import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set } from 'lodash/fp';

import { Manager } from './manager.model';
import { ManagerActionTypes, ManagerActions } from './manager.actions';
import { EntityStatus } from '../entities';
export interface ManagerEntityState extends EntityState<Manager> {
  status: EntityStatus;
  total: number;
  fieldsByManager: {
    [id: string]: {
      fields: {
        [name: string]: string[]
      },
    loadingAllTotalFields: boolean
    }
  };
  nodesByManager: {
    [id: string]: {
      nodes: string[],
      total: number,
      allTotal: number,
      loading: boolean,
      loadingAllTotal: boolean

    }
  };
  counter: number;
  nodestatus: EntityStatus;
  searchstatus: EntityStatus;
}

export const managerEntityAdapter: EntityAdapter<Manager> = createEntityAdapter<Manager>();

export const ManagerEntityInitialState: ManagerEntityState = managerEntityAdapter.getInitialState({
  status: EntityStatus.notLoaded,
  nodestatus: EntityStatus.notLoaded,
  searchstatus: EntityStatus.notLoaded,
  total: 0,
  fieldsByManager: {},
  nodesByManager: {},
  counter : 0
});

export function managerEntityReducer(state: ManagerEntityState = ManagerEntityInitialState,
                                     action: ManagerActions): ManagerEntityState {

  switch (action.type) {

    case ManagerActionTypes.SEARCH:
      const searchstatus = set('searchstatus', EntityStatus.loading, state);
      return set('status', EntityStatus.loading, searchstatus);

    case ManagerActionTypes.SEARCH_SUCCESS: {
      // tslint:disable-next-line: no-shadowed-variable
      const counter = set('counter', 0 , managerEntityAdapter.removeAll(state));
      const totalCount = set('total', action.payload.total, counter);
      // tslint:disable-next-line: no-shadowed-variable
      const searchstatus = set('searchstatus', EntityStatus.loadingSuccess, totalCount);
      return set('status', EntityStatus.loadingSuccess,
                managerEntityAdapter.setAll(action.payload.managers, searchstatus));
    }

    case ManagerActionTypes.SEARCH_FAILURE:
      return set('status', EntityStatus.loadingFailure, state);

    case ManagerActionTypes.SEARCH_FIELDS: {
      const {managerId} = action.payload;
      return set(`nodesByManager.${managerId}.loadingAllTotalFields`, true, state);
    }

    case ManagerActionTypes.SEARCH_FIELDS_SUCCESS: {
      const {managerId, field, fields} = action.payload;
      const fieldstatus = set(`nodesByManager.${managerId}.loadingAllTotalFields`, false, state);
      return set(`fieldsByManager.${managerId}.fields.${field}`, fields, fieldstatus);
    }

    case ManagerActionTypes.SEARCH_FIELDS_FAILURE: {
      const {managerId} = action.payload;
      return set(`nodesByManager.${managerId}.loadingAllTotalFields`, false, state);
    }

    case ManagerActionTypes.ALL_NODES_FAILURE: {
      const {managerId} = action.payload;
      return set(`nodesByManager.${managerId}.loadingAllTotal`, false, state);
    }

    case ManagerActionTypes.GET_NODES: {
      const {managerId} = action.payload;
      return set(`nodesByManager.${managerId}.loading`, true, state);
    }

    case ManagerActionTypes.GET_NODES_SUCCESS: {
      const {managerId, nodes} = action.payload;
      return set(`nodesByManager.${managerId}.nodes`, nodes,
        set(`nodesByManager.${managerId}.loading`, false, state));
    }

    case ManagerActionTypes.DELETE_NODES: {
      return set('status', EntityStatus.loading, state);
    }

    case ManagerActionTypes.DELETE_NODES_SUCCESS: {
      return set('status', EntityStatus.loadingSuccess, state);
    }

    case ManagerActionTypes.SEARCH_NODES: {
      const {managerId} = action.payload;
      return set(`nodesByManager.${managerId}.loading`, true, state);
    }

    case ManagerActionTypes.SEARCH_NODES_SUCCESS: {
      const {managerId, nodes} = action.payload;
      return set(`nodesByManager.${managerId}.nodes`, nodes,
        set(`nodesByManager.${managerId}.loading`, false, state));
    }

    case ManagerActionTypes.ALL_NODES: {
      const {managerId} = action.payload;
      return set(
        `nodesByManager.${managerId}.loadingAllTotal`,
        true,
        set(`nodesByManager.${managerId}.allTotal`, 0, state)
      );
    }

    case ManagerActionTypes.ALL_NODES_SUCCESS: {
      const {managerId, total} = action.payload;
      const nodestatus = set(`nodesByManager.${managerId}.loadingAllTotal`, false, state);
      return set(`nodesByManager.${managerId}.allTotal`, total, nodestatus);
    }

    case ManagerActionTypes.GET: {
      return set('status', EntityStatus.loading, state);
    }

    case ManagerActionTypes.GET_SUCCESS: {
      return set('status', EntityStatus.loadingSuccess,
                 managerEntityAdapter.addOne(action['payload'].manager, state));
    }

    case ManagerActionTypes.GET_FAILURE:
      return set('status', EntityStatus.loadingFailure, state);

    case ManagerActionTypes.CREATE:
      return set('status', EntityStatus.loading, state);

    case ManagerActionTypes.CREATE_SUCCESS:
      return set('status', EntityStatus.loadingSuccess, state);

    case ManagerActionTypes.CREATE_FAILURE:
      return set('status', EntityStatus.loadingFailure, state);

    case ManagerActionTypes.DELETE:
      return set('status', EntityStatus.loading, state);

    case ManagerActionTypes.DELETE_SUCCESS:
      return set('status', EntityStatus.loadingSuccess,
                 managerEntityAdapter.removeOne(action.payload.id, state));

    case ManagerActionTypes.DELETE_FAILURE:
      return set('status', EntityStatus.loadingFailure, state);

    case ManagerActionTypes.UPDATE:
      return set('status', EntityStatus.loading, state);

    case ManagerActionTypes.UPDATE_SUCCESS:
      return set('status', EntityStatus.loadingSuccess, state);

    case ManagerActionTypes.UPDATE_FAILURE:
      return set('status', EntityStatus.loadingFailure, state);

    case ManagerActionTypes.FIRST_LOAD:
      return ManagerEntityInitialState;

    default:
      return state;

  }
}
