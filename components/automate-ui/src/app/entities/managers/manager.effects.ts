import { catchError, withLatestFrom, mergeMap, map, filter } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { Actions, createEffect, ofType } from '@ngrx/effects';
import { of } from 'rxjs';
import { HttpErrorResponse } from '@angular/common/http';
import { toUpper } from 'lodash/fp';

import {
  ManagerActionTypes,
  ManagersSearch,
  ManagersSearchSuccess,
  ManagersSearchFailure,
  ManagersSearchSuccessPayload,
  ManagerGetNodes,
  ManagerGetNodesSuccess,
  ManagerGetNodesFailure,
  ManagerDeleteNodes,
  ManagerDeleteNodesSuccess,
  ManagerDeleteNodesFailure,
  ManagerSearchNodes,
  ManagerSearchNodesSuccess,
  ManagerSearchNodesFailure,
  ManagerAllNodes,
  ManagerAllNodesSuccess,
  ManagerAllNodesFailure,
  ManagerSearchFields,
  ManagerSearchFieldsSuccess,
  ManagerSearchFieldsFailure,
  CreateManager,
  CreateManagerSuccess,
  CreateManagerFailure,
  GetManager,
  GetManagerSuccess,
  GetManagerFailure,
  DeleteManager,
  DeleteManagerSuccess,
  DeleteManagerFailure,
  UpdateManager,
  UpdateManagerSuccess,
  UpdateManagerFailure,
  NavManagerDetail,
  NavManagerList,
  NavManagerEdit
} from './manager.actions';
import {
  ManagerRequests,
  ManagerGetNodesResponse,
  ManagerSearchNodesResponse,
  ManagerSearchFieldsResponse
} from './manager.requests';
import { CreateNotification } from '../notifications/notification.actions';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from '../../ngrx.reducers';
import {
  IntegrationsDetailState
} from '../../pages/integrations/detail/integrations-detail.reducer';
import { Type } from '../notifications/notification.model';
import { ROUTER_NAVIGATION, RouterNavigationAction } from '@ngrx/router-store';

@Injectable()
export class ManagerEffects {
  constructor(
    private actions$: Actions,
    private requests: ManagerRequests,
    private store: Store<NgrxStateAtom>
  ) {}

  routeManagerList$ = createEffect(() =>
    this.actions$.pipe(
    ofType(ROUTER_NAVIGATION),
    filter((action: RouterNavigationAction) => {
      return action.payload.routerState.url.split('?')[0] === '/settings/node-integrations';
    }),
    map((action: RouterNavigationAction) => {
      const queryParams = action.payload.routerState['queryParams'];
      const sort = queryParams['sort'];
      const order = toUpper(queryParams['order']) || undefined;
      return new NavManagerList({ sort, order });
    })
  ));

  routeManagerDetail$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ROUTER_NAVIGATION),
      filter((action: RouterNavigationAction) => {
        const { routerState } = action.payload;
        const url = routerState['url'];
        const params = routerState['params'];
        return url.startsWith('/settings/node-integrations') &&
          !url.startsWith('/settings/node-integrations/edit') &&
          params['id'];
      }),
      map((action: RouterNavigationAction) => {
        const { routerState } = action.payload;
        const managerId = routerState['params']['id'];
        const page = parseInt(routerState['queryParams']['page'], 10) || 1;
        const per_page = parseInt(routerState['queryParams']['per_page'], 10) || 100;
        return new NavManagerDetail({ managerId, page, per_page });
      })
    ));

  routeManagerEdit$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ROUTER_NAVIGATION),
      filter((action: RouterNavigationAction) => {
        const { routerState } = action.payload;
        const url = routerState['url'];
        const params = routerState['params'];
        return url.startsWith('/settings/node-integrations/edit') && params['id'];
      }),
      map((action: RouterNavigationAction) => {
        const { routerState } = action.payload;
        const managerId = routerState['params']['id'];
        return new NavManagerEdit({ managerId });
      })
    ));

  navManagerList$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ManagerActionTypes.NAV_LIST),
      map((action: NavManagerList) => {
        return new ManagersSearch(action.payload);
      })
    ));

  navManagerDetail$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ManagerActionTypes.NAV_DETAIL),
      mergeMap((action: NavManagerDetail) => {
        const { managerId, page, per_page } = action.payload;
        return [
          new GetManager({ id: managerId }),
          new ManagerGetNodes({ managerId, page, per_page })
        ];
      })
    ));

  navManagerEdit$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ManagerActionTypes.NAV_EDIT),
      map((action: NavManagerEdit) => {
        const { managerId } = action.payload;
        return new GetManager({ id: managerId });
      })
    ));

  managersSearch$ = createEffect(() =>
    this.actions$.pipe(
    ofType(ManagerActionTypes.SEARCH),
    mergeMap((action: ManagersSearch) =>
              this.requests.search(action.payload).pipe(
              map((payload: ManagersSearchSuccessPayload) => new ManagersSearchSuccess(payload)),
              catchError((error: HttpErrorResponse) => of(new ManagersSearchFailure(error)))))));

  managersSearchFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ManagerActionTypes.SEARCH_FAILURE),
      map(({ payload }: ManagersSearchFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get managers: ${msg || payload.error}`
        });
      })));

  managerGetNodes$ = createEffect(() =>
    this.actions$.pipe(
    ofType(ManagerActionTypes.GET_NODES),
    mergeMap((action: ManagerGetNodes) =>
              this.requests.getNodes(action.payload).pipe(
              map((resp: ManagerGetNodesResponse) =>
                   new ManagerGetNodesSuccess(Object.assign(resp, action.payload))),
              catchError((error: HttpErrorResponse) =>
                     of(new ManagerGetNodesFailure(error)))))));

  managerGetNodesFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ManagerActionTypes.GET_NODES_FAILURE),
      map(({ payload }: ManagerGetNodesFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get manager nodes: ${msg || payload.error}`
        });
      })));

  deleteNodes$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ManagerActionTypes.DELETE_NODES),
      mergeMap((action: ManagerDeleteNodes) =>
        this.requests.deleteNodes(action.payload.ids)),
      map(_success => new ManagerDeleteNodesSuccess()),
      catchError((error) => of(new ManagerDeleteNodesFailure(error)))
    ));

  deleteNodesSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ManagerActionTypes.DELETE_NODES_SUCCESS),
      withLatestFrom(this.store),
      map(([_action, storeState]) => {
        const integrationsDetailState: IntegrationsDetailState = storeState.integrations_detail;
        const managerId = integrationsDetailState.manager.id;
        const page = integrationsDetailState.managerNodesPage;
        const per_page = integrationsDetailState.managerNodesPerPage;
        return new ManagerGetNodes({ managerId, page, per_page });
      })));

  managerSearchNodes$ = createEffect(() =>
    this.actions$.pipe(
    ofType(ManagerActionTypes.SEARCH_NODES),
    mergeMap((action: ManagerSearchNodes) =>
              this.requests.searchNodes(action.payload).pipe(
              map((resp: ManagerSearchNodesResponse) =>
                   new ManagerSearchNodesSuccess(Object.assign(resp, action.payload))),
              catchError((error: HttpErrorResponse) =>
                     of(new ManagerSearchNodesFailure({...action.payload, error})))))));

  managerSearchNodesFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ManagerActionTypes.SEARCH_NODES_FAILURE),
      map(({ payload }: ManagerSearchNodesFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get manager nodes: ${msg || payload.error}`
        });
      })));

  managerAllNodes$ = createEffect(() =>
    this.actions$.pipe(
    ofType(ManagerActionTypes.ALL_NODES),
    mergeMap((action: ManagerAllNodes) => {
      return this.requests.searchNodes(action.payload).pipe(
        map((resp: ManagerSearchNodesResponse) =>
             new ManagerAllNodesSuccess(Object.assign(resp, action.payload))),
        catchError((error: HttpErrorResponse) =>
               of(new ManagerAllNodesFailure({...action.payload, error}))));
    })));

  managerAllNodesFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ManagerActionTypes.ALL_NODES_FAILURE),
      map(({ payload }: ManagerAllNodesFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get manager nodes: ${msg || payload.error}`
        });
      })));

  managerSearchFields$ = createEffect(() =>
    this.actions$.pipe(
    ofType(ManagerActionTypes.SEARCH_FIELDS),
    mergeMap((action: ManagerSearchFields) =>
              this.requests.searchFields(action.payload).pipe(
              map((resp: ManagerSearchFieldsResponse) =>
                   new ManagerSearchFieldsSuccess(Object.assign(resp, action.payload))),
              catchError((error: HttpErrorResponse) =>
                     of(new ManagerSearchFieldsFailure({...action.payload, error})))))));

  managerSearchFieldsFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ManagerActionTypes.SEARCH_FIELDS_FAILURE),
      map(({ payload }: ManagerSearchFieldsFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get manager fields: ${msg || payload.error}`
        });
      })));

  fetchManager$ = createEffect(() =>
    this.actions$.pipe(
    ofType(ManagerActionTypes.GET),
    mergeMap((action: GetManager) =>
              this.requests.fetch(action.payload).pipe(
              map((resp) => new GetManagerSuccess({manager: resp})),
              catchError((error: HttpErrorResponse) => of(new GetManagerFailure(error)))))));

  fetchManagerFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ManagerActionTypes.GET_FAILURE),
      map(({ payload }: GetManagerFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get node manager: ${msg || payload.error}`
        });
      })));

  deleteManager$ = createEffect(() =>
    this.actions$.pipe(
    ofType(ManagerActionTypes.DELETE),
    mergeMap(({payload}: DeleteManager) =>
      this.requests.delete(payload).pipe(
        map((_resp) => new DeleteManagerSuccess({ id: payload.id })),
        catchError((error: HttpErrorResponse) => of(new DeleteManagerFailure(error)))))));

  deleteManagerSuccess$ = createEffect(() =>
    this.actions$.pipe(
    ofType(ManagerActionTypes.DELETE_SUCCESS),
    map(() => new CreateNotification({
      type: Type.info,
      message: 'Successfully deleted node manager'
    }))));

  deleteManagerFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ManagerActionTypes.DELETE_FAILURE),
      map(({ payload }: DeleteManagerFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not delete node manager: ${msg || payload.error}`
        });
      })));

  createManager$ = createEffect(() =>
    this.actions$.pipe(
    ofType(ManagerActionTypes.CREATE),
    mergeMap((action: CreateManager) => {
      return this.requests.create(action.payload).pipe(
        map((_resp) => new CreateManagerSuccess()),
        catchError((error) => of(new CreateManagerFailure(error))));
    })));

  createManagerSuccess$ = createEffect(() =>
    this.actions$.pipe(
    ofType(ManagerActionTypes.CREATE_SUCCESS),
    map(() => new CreateNotification({
      type: Type.info,
      message: 'Successfully created new node manager'
    }))));

  createManagerFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ManagerActionTypes.CREATE_FAILURE),
      map(({ payload }: CreateManagerFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not create node manager: ${msg || payload.error}`
        });
      })));

  updateManager$ = createEffect(() =>
    this.actions$.pipe(
    ofType(ManagerActionTypes.UPDATE),
    mergeMap((action: UpdateManager) =>
              this.requests.update(action.payload).pipe(
              map((_resp) => new UpdateManagerSuccess()),
              catchError((error: HttpErrorResponse) => of(new UpdateManagerFailure(error)))))));

  updateManagerFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ManagerActionTypes.UPDATE_FAILURE),
      map(({ payload }: UpdateManagerFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not update node manager: ${msg || payload.error}`
        });
      })));
}
