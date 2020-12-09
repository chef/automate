import { Component, Input, ViewChild, ElementRef, OnDestroy } from '@angular/core';
import { capitalize, getOr, endsWith, replace, concat } from 'lodash/fp';
import { Subject, Observable } from 'rxjs';
import { map, takeUntil } from 'rxjs/operators';
import { ChefEvent, ChefEventCollection, EventFeedFilter, Chicklet } from '../../types/types';
import { EventFeedService } from '../../services/event-feed/event-feed.service';
import * as moment from 'moment/moment';
import { DateTime } from 'app/helpers/datetime/datetime';

const ENTITY_TYPE_TAG = 'event-type';
@Component({
  selector: 'app-event-feed-table',
  templateUrl: './event-feed-table.component.html',
  styleUrls: ['./event-feed-table.component.scss']
})

export class EventFeedTableComponent implements OnDestroy {
  @Input() events: ChefEvent[];
  // The currently set collection of searchbar filters
  @Input() searchBarFilters: Chicklet[];
  showEventGroupPanel = false;
  groupedEvent: ChefEvent;
  groupedEvents: ChefEvent[];
  groupedEventsButton;

  DateTime = DateTime;

  @ViewChild('groupSidePanel', { static: true }) sidepanel: ElementRef;
  private isDestroyed = new Subject<boolean>();

  constructor(
    private eventFeedService: EventFeedService
  ) { }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  isGroup(event: ChefEvent): boolean {
    return event.eventCount > 1;
  }

  displayGroupedEvents(event: ChefEvent, clickEvent) {
    this.groupedEvent = null;
    this.groupedEvents = [];
    this.groupedEvent = event;
    this.sidepanel.nativeElement.focus();
    this.groupedEventsButton = document.getElementById(clickEvent.target.id);

    this.getGroupedEvents(event)
      .pipe(takeUntil(this.isDestroyed))
      .subscribe((events: ChefEvent[]) => {
      this.groupedEvents = events;
    });

    this.showEventGroupPanel = true;
  }

  getGroupedEvents(event: ChefEvent): Observable<ChefEvent[]> {
    const searchBar = concat({ type: ENTITY_TYPE_TAG, text: event.eventType },
      this.searchBarFilters.filter((f: Chicklet) => f.type !== ENTITY_TYPE_TAG));

    const filter: EventFeedFilter = {
      startDate: moment(event.startTime),
      endDate: moment(event.endTime),
      requestorName: event.requestorName,
      searchBar: searchBar,
      task: event.task,
      collapse: false,
      pageSize: event.eventCount
    };

    return this.eventFeedService.getEventFeed(filter).pipe(map(
      (eventCollection: ChefEventCollection) => eventCollection.events));
  }

  hideGroupedEvents() {
    this.showEventGroupPanel = false;
    this.groupedEventsButton.focus();
  }

  clickedOutsidePanel() {
    if (this.showEventGroupPanel) {
      this.hideGroupedEvents();
    }
  }

  displayRequestorName(requestor: string): string {
    if (this.displayRequestor(requestor)) {
      return requestor;
    }
    return '';
  }

  displayRequestorPreposition(requestor: string): string {
    if (this.displayRequestor(requestor)) {
      return ' by ';
    }
    return '';
  }

  getEventTypeLabel(event: ChefEvent): string {
    const labelMap = {
      version: 'cookbook',
      cookbook_artifact_version: 'cookbook',
      item: 'data bag item',
      bag: 'data bag',
      scanjobs: 'scan job'
    };
    const label = getOr(event.eventType, event.eventType, labelMap);

    if (this.isGroup(event)) {
      return this.pluralize(label);
    }

    return label;
  }

  pluralize(str) {
    if (str === 'key') {
      return 'keys';
    }
    if (endsWith('y', str)) {
      return replace(/y$/, 'ies', str);
    }

    return str + 's';
  }

  getFormattedEventType(event: ChefEvent): string {
    switch (event.task) {
      case 'delete':
        return 'deleted';
      case 'edit':
        return 'edited';
      case 'update':
        return 'updated';
      case 'create':
        return 'created';
      default:
        return event.task;
    }
  }

  getEventDescription(event: ChefEvent): string {
    let text = `${capitalize(this.getEventTypeLabel(event))} `;
    switch (event.eventType) {
      case 'version':
        text += `<b>${event.parentName}</b>, version <b>${event.entityName}</b>`;
        break;
      // TODO @afiune format other event types like a data bag item
      // case 'item':
      case 'cookbook_artifact_version':
        text += `<b>${event.parentName}</b>, version <b>${event.entityName}</b>`;
        break;
      default:
        text += `<b>${event.entityName}</b>`;
    }

    text += ` ${this.getFormattedEventType(event)}`;

    if (this.displayRequestor(event.requestorName)) {
      text += ` by <b>${event.requestorName}</b>`;
    }

    return text;
  }

  displayRequestor(name: string): boolean {
    return (name !== 'User' && name !== 'UI User');
  }

  getEventGroupText(event: ChefEvent): string {
    switch (event.eventType) {
      case 'version':
        return `${event.parentName}: v${event.entityName}`;
      // TODO @afiune format other event types like a data bag item
      // case 'item':
      case 'cookbook_artifact_version':
        return `${event.parentName}: v${event.entityName}`;
      default:
        return event.entityName;
    }
  }
}
