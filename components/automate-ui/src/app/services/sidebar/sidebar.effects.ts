import { iif as observableIif, of as observableOf } from 'rxjs';

import { withLatestFrom, switchMap, catchError, mergeMap, filter, map } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { Actions, Effect, ofType } from '@ngrx/effects';
import * as sidebarActions from './sidebar.actions';
import * as eventFeedActions from '../event-feed/event-feed.actions';
import { SidebarService } from './sidebar.service';
import { ROUTER_NAVIGATION, RouterNavigationAction } from '@ngrx/router-store';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from '../../ngrx.reducers';
import { GetNodes, GetNodeCount } from '../../entities/client-runs/client-runs.actions';

@Injectable()
export class SidebarEffects {
  constructor(
    private actions$: Actions,
    private sidebarService: SidebarService,
    private store: Store<NgrxStateAtom>
  ) {
  }

  @Effect()
  navToSidebar$ = this.actions$.pipe(
    ofType(ROUTER_NAVIGATION),
    map((action: RouterNavigationAction) => action.payload.routerState.url),
    filter(path => path.startsWith('/event-feed') || path.startsWith('/client-runs')),
    mergeMap(_path => [
      sidebarActions.getOrgs(),
      sidebarActions.getChefServers()]));

  @Effect()
  getOrgs$ = this.actions$.pipe(
    ofType(sidebarActions.GET_ORGS),
    switchMap((_action) => {
      return this.sidebarService.getOrgs().pipe(
      map(sidebarActions.getOrgsSuccess),
      catchError(() => observableOf({type: sidebarActions.GET_ORGS_ERROR})));
    }));

  @Effect()
  getOrgsSuccess$ = this.actions$.pipe(
    ofType(sidebarActions.GET_ORGS_SUCCESS),
    withLatestFrom(this.store),
    switchMap(([_action, storeState]) => {
      const selectedOrgs = storeState.sidebar.selectedOrgs;
      const allOrgs = storeState.sidebar.allOrgs;

      const availableSelectedOrgs = selectedOrgs.filter(
        (selectedOrg: string) => allOrgs.indexOf(selectedOrg) >= 0);

      const areThereSelectedOrgsDeleted = availableSelectedOrgs.length !== selectedOrgs.length;

      return observableIif(
        () => areThereSelectedOrgsDeleted,
        observableOf(sidebarActions.setSelectedSidebarOrgs(availableSelectedOrgs)),
        observableOf());
    }));

  @Effect()
  getChefServers$ = this.actions$.pipe(
    ofType(sidebarActions.GET_CHEF_SERVERS),
    switchMap((_action) => {
      return this.sidebarService.getChefServers().pipe(
      map(sidebarActions.getChefServersSuccess),
      catchError(() => observableOf({type: sidebarActions.GET_CHEF_SERVERS_ERROR})));
    }));

  @Effect()
  getChefServersSuccess$ = this.actions$.pipe(
    ofType(sidebarActions.GET_CHEF_SERVERS_SUCCESS),
    withLatestFrom(this.store),
      switchMap(([_action, storeState]) => {
        const selectedChefServers = storeState.sidebar.selectedChefServers;
        const allChefServers = storeState.sidebar.allChefServers;

        const availableSelectedChefServers = selectedChefServers.filter(
          (selectedServer: string) => allChefServers.indexOf(selectedServer) >= 0);

        const areThereSelectedChefServersDeleted =
          availableSelectedChefServers.length !== selectedChefServers.length;

        return observableIif(
          () => areThereSelectedChefServersDeleted,
          observableOf(sidebarActions.setSelectedSidebarChefServers(availableSelectedChefServers)),
          observableOf());
      }));

  @Effect()
  setSelectedSidebarOrgs$ = this.actions$.pipe(
    ofType(sidebarActions.SET_SELECTED_SIDEBAR_ORGS),
    mergeMap((_action: sidebarActions.SidebarAction) =>
      [
        eventFeedActions.getInitialFeed(),
        eventFeedActions.getGuitarStrings(),
        eventFeedActions.getTypeCounts(),
        eventFeedActions.getTaskCounts(),
        new GetNodes(),
        new GetNodeCount()
      ]));

  @Effect()
  setSelectedSidebarChefServers$ = this.actions$.pipe(
    ofType(sidebarActions.SET_SELECTED_SIDEBAR_CHEF_SERVERS),
    mergeMap((_action: sidebarActions.SidebarAction) =>
      [
        eventFeedActions.getInitialFeed(),
        eventFeedActions.getGuitarStrings(),
        eventFeedActions.getTypeCounts(),
        eventFeedActions.getTaskCounts(),
        new GetNodes(),
        new GetNodeCount()
      ]));
}
