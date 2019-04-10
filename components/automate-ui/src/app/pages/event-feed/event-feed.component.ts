import { takeUntil } from 'rxjs/operators';
import { Component, OnInit, OnDestroy, ViewChild } from '@angular/core';
import { Subject } from 'rxjs';

import {
  ChefEvent,
  EventTypeCount,
  EventTaskCount,
  DateRange,
  GuitarStringCollection
} from '../../types/types';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from '../../ngrx.reducers';
import * as eventFeedSelectors from '../../services/event-feed/event-feed.selectors';
import * as eventFeedActions from '../../services/event-feed/event-feed.actions';
import { Status } from '../../services/event-feed/event-feed.reducer';
import { sumBy } from 'lodash';
import { initialState } from '../../services/event-feed/event-feed.reducer';
import * as moment from 'moment';

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
  @ViewChild('guitarStrings') guitarStrings;
  resetTimescaleDisabled = true;

  constructor(
    private store: Store<NgrxStateAtom>
  ) {}

  ngOnInit() {
    this.store.select(eventFeedSelectors.loadedEvents).pipe(
    takeUntil(this.isDestroyed))
    .subscribe((loadedEvents: ChefEvent[]) => {
        this.events = loadedEvents;
        this.totalNumberOfEventsLoaded = this.countTotalNumberOfEvents(loadedEvents);
        this.loadedEmptySetOfEvents = this.events.length === 0 &&
          this.initialFeedStatus === Status.loadingSuccess;
    });

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

  selectEventType(event) {
    const value = event.target.value;
    this.updateSelectionBox(value);
    switch (value) {
      case 'bag':
        this.store.dispatch(eventFeedActions.addFeedFilter('entityType', ['item', value]));
        break;
      case 'cookbook':
        this.store.dispatch(eventFeedActions.addFeedFilter('entityType', ['version', value]));
        break;
      default:
        this.store.dispatch(eventFeedActions.addFeedFilter('entityType', value));
        break;
    }
  }

  private countTotalNumberOfEvents(loadedEvents: ChefEvent[]): number {
    return sumBy(loadedEvents, (event) => event.eventCount);
  }

  private updateSelectionBox(entityType: string): void {
    if (entityType !== undefined && entityType !== this.selectedEntityType) {
      this.selectedEntityType = entityType;
    }
  }
}
