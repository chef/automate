import { of as observableOf } from 'rxjs';

import { withLatestFrom, catchError, switchMap, mergeMap, filter, map } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { Actions, Effect, ofType } from '@ngrx/effects';
import * as actions from './event-feed.actions';
import { EventFeedService } from './event-feed.service';
import { ROUTER_NAVIGATION, RouterNavigationAction } from '@ngrx/router-store';
import { EventFeedState } from './event-feed.reducer';
import { SidebarState } from '../sidebar/sidebar.reducer';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from '../../ngrx.reducers';
import { SidebarFilter } from '../../types/types';
import { HttpErrorResponse } from '@angular/common/http';

@Injectable()
export class EventFeedEffects {
  constructor(
    private actions$: Actions,
    private eventFeedService: EventFeedService,
    private store: Store<NgrxStateAtom>
  ) {}

  @Effect()
  navToEventFeed$ = this.actions$.pipe(
    ofType(ROUTER_NAVIGATION),
    map((action: RouterNavigationAction) => action.payload.routerState.url),
    filter(path => path.startsWith('/event-feed')),
    map((_path) => actions.getInitialEventFeedLoad()));

  @Effect()
  getInitialEventFeedLoad$ =  this.actions$.pipe(
    ofType(actions.GET_INITIAL_EVENT_FEED_LOAD),
    mergeMap((_action: actions.EventFeedAction) =>
      [actions.getInitialFeed(), actions.getTaskCounts(),
        actions.getGuitarStrings(), actions.getTypeCounts()]));

  @Effect()
  getInitialFeed$ = this.actions$.pipe(
    ofType(actions.GET_INITIAL_FEED),
    withLatestFrom(this.store),
    switchMap(([_action, storeState]) => {
      const eventFeedState: EventFeedState = storeState.event_feed;
      const sidebarState: SidebarState = storeState.sidebar;
      const sidebarFilter: SidebarFilter = {
        organizations: Array.from(sidebarState.selectedOrgs),
        servers: Array.from(sidebarState.selectedChefServers)
      };

      return this.eventFeedService.getEventFeed(eventFeedState.filters, sidebarFilter).pipe(
      map(actions.getInitialFeedSuccess),
      catchError((error: HttpErrorResponse) => {
        if (error.status === 403) {
          return observableOf({type: actions.GET_INITIAL_FEED_PERMISSION_DENIED});
        } else {
          return observableOf({type: actions.GET_INITIAL_FEED_ERROR});
        }
      }));
    }));

  @Effect()
  loadMoreFeed$ = this.actions$.pipe(
    ofType(actions.LOAD_MORE_FEED),
    withLatestFrom(this.store),
    switchMap(([_action, storeState]) => {
      const eventFeedState: EventFeedState = storeState.event_feed;
      const sidebarState: SidebarState = storeState.sidebar;
      const events = eventFeedState.loadedEvents;
      const lastEvent = events[events.length - 1];
      const sidebarFilter: SidebarFilter = {
        organizations: Array.from(sidebarState.selectedOrgs),
        servers: Array.from(sidebarState.selectedChefServers)
      };

      return this.eventFeedService.loadMoreEventFeed(eventFeedState.filters,
        sidebarFilter, lastEvent).pipe(
      map(actions.loadMoreFeedSuccess),
      catchError(() => observableOf({type: actions.LOAD_MORE_FEED_ERROR})));
    }));

  @Effect()
  getGuitarStrings$ = this.actions$.pipe(
    ofType(actions.GET_GUITAR_STRINGS),
    withLatestFrom(this.store),
    switchMap(([_action, storeState]) => {
      const eventFeedState: EventFeedState = storeState.event_feed;
      const sidebarState: SidebarState = storeState.sidebar;
      const sidebarFilter: SidebarFilter = {
        organizations: Array.from(sidebarState.selectedOrgs),
        servers: Array.from(sidebarState.selectedChefServers)
      };

      return this.eventFeedService.getGuitarStrings(eventFeedState.filters, sidebarFilter).pipe(
      map(actions.getGuitarStringsSuccess),
      catchError(() => observableOf({type: actions.GET_GUITAR_STRINGS_ERROR})));
    }));

  @Effect()
  getTypeCounts$ = this.actions$.pipe(
    ofType(actions.GET_TYPE_COUNTS),
    withLatestFrom(this.store),
    switchMap(([_action, storeState]) => {
      const eventFeedState: EventFeedState = storeState.event_feed;
      const sidebarState: SidebarState = storeState.sidebar;
      const sidebarFilter: SidebarFilter = {
        organizations: Array.from(sidebarState.selectedOrgs),
        servers: Array.from(sidebarState.selectedChefServers)
      };

      return this.eventFeedService.getEventTypeCount(eventFeedState.filters, sidebarFilter).pipe(
      map(actions.getTypeCountsSuccess),
      catchError(() => observableOf({type: actions.GET_TYPE_COUNTS_ERROR})));
    }));

  @Effect()
  getTaskCounts$ = this.actions$.pipe(
    ofType(actions.GET_TASK_COUNTS),
    withLatestFrom(this.store),
    switchMap(([_action, storeState]) => {
      const eventFeedState: EventFeedState = storeState.event_feed;
      const sidebarState: SidebarState = storeState.sidebar;
      const sidebarFilter: SidebarFilter = {
        organizations: Array.from(sidebarState.selectedOrgs),
        servers: Array.from(sidebarState.selectedChefServers)
      };

      return this.eventFeedService.getEventTaskCount(eventFeedState.filters, sidebarFilter).pipe(
      map(actions.getTaskCountsSuccess),
      catchError(() => observableOf({type: actions.GET_TASK_COUNTS_ERROR})));
    }));

  @Effect()
  addFeedFilter$ = this.actions$.pipe(
    ofType(actions.ADD_FEED_FILTER),
    mergeMap((_action: actions.EventFeedAction) =>
      [actions.getInitialFeed(), actions.getGuitarStrings(), actions.getTaskCounts()]));

  @Effect()
  addFeedDateRangeFilter$ = this.actions$.pipe(
    ofType(actions.ADD_FEED_DATE_RANGE_FILTER),
    mergeMap((_action: actions.EventFeedAction) =>
      [actions.getInitialFeed(), actions.getTaskCounts(), actions.getGuitarStrings()]));
}
