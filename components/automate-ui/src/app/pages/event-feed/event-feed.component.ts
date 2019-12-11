import { takeUntil, map } from 'rxjs/operators';
import { Component, OnInit, OnDestroy, ViewChild } from '@angular/core';
import { Subject, Observable } from 'rxjs';

import {
  ChefEvent,
  EventTypeCount,
  EventTaskCount,
  DateRange,
  GuitarStringCollection
} from '../../types/types';
import { Store, createSelector } from '@ngrx/store';
import { NgrxStateAtom } from '../../ngrx.reducers';
import * as eventFeedSelectors from '../../services/event-feed/event-feed.selectors';
import * as eventFeedActions from '../../services/event-feed/event-feed.actions';
import { ActivatedRoute, Router, ParamMap } from '@angular/router';
import { Status } from '../../services/event-feed/event-feed.reducer';
import { Chicklet, SearchBarCategoryItem } from '../../types/types';
import { sumBy } from 'lodash';
import { initialState } from '../../services/event-feed/event-feed.reducer';
import * as moment from 'moment';
import { some, pickBy } from 'lodash/fp';
import {
  eventFeedState
} from '../../services/event-feed/event-feed.selectors';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';

@Component({
  selector: 'app-event-feed',
  templateUrl: './event-feed.component.html',
  styleUrls: ['./event-feed.component.scss']
})

export class EventFeedComponent implements OnInit, OnDestroy {
  events: ChefEvent[] = new Array<ChefEvent>();
  selectedEntityType = 'total';
  private isDestroyed = new Subject<boolean>();
  eventTypeCount = EventTypeCount.Null;
  updateCounts = 0;
  createCounts = 0;
  deleteCounts = 0;
  totalTaskCounts = 0;
  displayEventType: string;
  completeNumberOfEvents = 0;
  totalNumberOfEventsLoaded = 0;
  initialFeedStatus = Status.notLoaded;
  eventsLoading = false;
  errorLoadingEvents = false;
  loadedEmptySetOfEvents = false;
  permissionDenied = false;
  guitarStringCollection: GuitarStringCollection = initialState.guitarStringCollection;
  @ViewChild('guitarStrings', { static: true }) guitarStrings;
  resetTimescaleDisabled = true;

  // Should the search bar filter bar be displayed
  filtersVisible = true;

  // The currently set collection of searchbar filters
  searchBarFilters$: Observable<Chicklet[]>;

  // The number of currently set searchbar filters
  numberOfSearchBarFilters$: Observable<number>;

  // autocomplete suggestions
  suggestions$: Observable<any[]>;

  // The catagories allowed for searching
  categoryTypes: SearchBarCategoryItem[] = [
    {
      type: 'organization',
      text: 'Chef Organization',
      allowWildcards: false
    },
    {
      type: 'chef_server',
      text: 'Chef Server',
      allowWildcards: false
    },
    {
      type: 'event-type',
      text: 'Event Type',
      allowWildcards: false,
      providedValues: [
        {name: 'client', title: 'Clients', icon: 'assignment_ind'},
        {name: 'cookbook', title: 'Cookbooks', icon: 'chrome_reader_mode'},
        {name: 'bag', title: 'Data Bags', icon: 'business_center'},
        {name: 'environment', title: 'Environments', icon: 'public'},
        {name: 'group', title: 'Groups', icon: 'people'},
        {name: 'node', title: 'Nodes', icon: 'storage'},
        {name: 'organization', title: 'Organizations', icon: 'layers'},
        {name: 'permission', title: 'Permissions', icon: 'lock_open'},
        {name: 'policyfile', title: 'Policyfiles', icon: 'add_to_photos'},
        {name: 'profile', title: 'Profiles', icon: 'library_books'},
        {name: 'role', title: 'Roles', icon: 'book'},
        {name: 'scanjobs', title: 'Scan Jobs', icon: 'wifi_tethering'},
        {name: 'user', title: 'Users', icon: 'person'}
      ]
    }
  ];

  toggleFilters() {
    this.filtersVisible = !this.filtersVisible;
  }

  onSuggestValues(event) {
    this.store.dispatch(eventFeedActions.getSuggestions(event.detail.type, event.detail.text));
  }

  onFilterAdded(event) {
    const {type, text} = event.detail;

    if (some({type}, this.categoryTypes) ) {
      const {queryParamMap} = this.route.snapshot;
      const queryParams = {...this.route.snapshot.queryParams};
      const values = queryParamMap.getAll(type).filter(value => value !== text).concat(text);

      queryParams[type] = values;

      this.router.navigate([], {queryParams});
    }
  }

  onFiltersClear(_event) {
    const queryParams = {...this.route.snapshot.queryParams};

    const filteredParams = pickBy((_value, key) => {
        return !some({'type': key}, this.categoryTypes);
      }, queryParams);

    delete filteredParams['page'];

    this.router.navigate([], {queryParams: filteredParams});
  }

  onFilterRemoved(event) {
    const {type, text} = event.detail;
    const {queryParamMap} = this.route.snapshot;
    const queryParams = {...this.route.snapshot.queryParams};
    const values = queryParamMap.getAll(type).filter(value => value !== text);

    if (values.length === 0) {
      delete queryParams[type];
    } else {
      queryParams[type] = values;
    }

    this.router.navigate([], {queryParams});
  }

  constructor(
    private store: Store<NgrxStateAtom>,
    private route: ActivatedRoute,
    private router: Router,
    private layoutFacade: LayoutFacadeService
  ) {}

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Dashboards);
    this.store.select(eventFeedSelectors.loadedEvents).pipe(
    takeUntil(this.isDestroyed))
    .subscribe((loadedEvents: ChefEvent[]) => {
        this.events = loadedEvents;
        this.totalNumberOfEventsLoaded = this.countTotalNumberOfEvents(loadedEvents);
        this.loadedEmptySetOfEvents = this.events.length === 0 &&
          this.initialFeedStatus === Status.loadingSuccess;
    });

    const allUrlParameters$ = this.getAllUrlParameters();

    this.searchBarFilters$ = allUrlParameters$.pipe(map((chicklets: Chicklet[]) =>
      chicklets.filter(chicklet => some({'type': chicklet.type}, this.categoryTypes))));

    this.numberOfSearchBarFilters$ = this.searchBarFilters$.pipe(
        map((chicklets: Chicklet[]) => chicklets.length));

    // URL change listener
    this.searchBarFilters$.pipe(takeUntil(this.isDestroyed)).subscribe(
      searchBarFilters => {
        this.store.dispatch(eventFeedActions.addSearchbarFilters(searchBarFilters));
      });

    this.suggestions$ = this.store.select(createSelector(eventFeedState,
          (state) => state.suggestions));

    this.store.select(eventFeedSelectors.completeNumberOfEvents).pipe(
      takeUntil(this.isDestroyed))
      .subscribe(completeNumberOfEvents => {
        this.completeNumberOfEvents = completeNumberOfEvents;
      });

    this.store.select(eventFeedSelectors.eventTypeCounts).pipe(
      takeUntil(this.isDestroyed))
      .subscribe(counts => this.eventTypeCount = counts);

    this.store.select(eventFeedSelectors.eventTaskCounts).pipe(
      takeUntil(this.isDestroyed))
      .subscribe((counts: EventTaskCount) => {
        this.totalTaskCounts = counts.total;
        this.updateCounts = counts.update;
        this.createCounts = counts.create;
        this.deleteCounts = counts.delete;
    });

    this.store.select(eventFeedSelectors.initialFeedStatus).pipe(
      takeUntil(this.isDestroyed))
      .subscribe(initialFeedStatus => {
        this.loadedEmptySetOfEvents = this.events.length === 0 &&
          initialFeedStatus === Status.loadingSuccess;
        this.permissionDenied = initialFeedStatus === Status.loadingPermissionDenied;
        this.errorLoadingEvents = initialFeedStatus === Status.loadingFailure;

        this.eventsLoading =  initialFeedStatus === Status.loading;
      });

    this.store.select(eventFeedSelectors.guitarStringCollection).pipe(
      takeUntil(this.isDestroyed))
      .subscribe((guitarStringCollection: GuitarStringCollection) =>
        this.guitarStringCollection = guitarStringCollection);
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  loadMore() {
    this.store.dispatch(eventFeedActions.loadMoreFeed());
  }

  selectDateRange(dateRange: DateRange): void {
    const start = moment(dateRange.start);
    const end = moment(dateRange.end);

    if (start.add(6, 'days').format('l') !== end.format('l')) {
      this.resetTimescaleDisabled = false;
    } else {
      this.resetTimescaleDisabled = true;
    }

    this.store.dispatch(eventFeedActions.addFeedDateRangeFilter(dateRange.start, dateRange.end));
  }

  getDisplayEventType(eventType) {
    switch (eventType) {
      case '': return 'events';
      case 'bag': return 'data bags';
      case 'item': return 'data bag items';
      case 'policyfile': return 'policy files';
      case 'scanjobs': return 'scan jobs';
      default: return eventType + 's';
    }
  }

  private countTotalNumberOfEvents(loadedEvents: ChefEvent[]): number {
    return sumBy(loadedEvents, (event) => event.eventCount);
  }

  private getAllUrlParameters(): Observable<Chicklet[]> {
    return this.route.queryParamMap.pipe(map((params: ParamMap) => {
      return params.keys.reduce((list, key) => {
        const paramValues = params.getAll(key);
        return list.concat(paramValues.map(value => ({type: key, text: value})));
      }, []);
    }));
  }
}
