import { map } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { Observable, of as observableOf } from 'rxjs';

import { reduce, includes, concat } from 'lodash/fp';
import { sortBy } from 'lodash';
import { ChefEvent,
  RespChefEvent,
  RespEventCollection,
  EventFeedFilter,
  RespChefEventCollection,
  GuitarString,
  GuitarStringItem,
  ResponseGuitarStringCollection,
  GuitarStringCollection,
  RespEventCounts,
  EventTypeCount,
  EventTaskCount,
  ChefEventCollection,
  Chicklet
} from '../../types/types';
import { initialState } from './event-feed.reducer';
import { environment } from '../../../environments/environment';
import * as moment from 'moment';
const GATEWAY_URL = environment.gateway_url;
const CONFIG_MGMT_URL = environment.config_mgmt_url;
const ENTITY_TYPE_TAG = 'entity_type';
const ENTITY_TYPE_DATA_BAG_ITEM_TAG = 'item';
const ENTITY_TYPE_DATA_BAG_TAG = 'bag';
const ENTITY_TYPE_COOKBOOK_TAG = 'cookbook';
const ENTITY_TYPE_COOKBOOK_VERSION_TAG = 'version';

@Injectable()
export class EventFeedService {
  constructor(
    private httpClient: HttpClient
  ) {}

  getEventFeed(filters: EventFeedFilter): Observable<ChefEventCollection> {
    const url = `${GATEWAY_URL}/eventfeed`;

    const options = {
      params: this.buildEventFeedURLSearchParams(filters, null)
    };

    return this.httpClient
      .get<RespChefEventCollection>(url, options).pipe(
      map((res) => this.convertResponseToChefEvents(res)));
  }

  loadMoreEventFeed(filters: EventFeedFilter,
    lastEvent: ChefEvent): Observable<ChefEventCollection> {
    const url = `${GATEWAY_URL}/eventfeed`;

    const options = {
      params: this.buildEventFeedURLSearchParams(filters, lastEvent)
    };

    return this.httpClient
      .get<RespChefEventCollection>(url, options).pipe(
      map((res) => this.convertResponseToChefEvents(res)));
  }

  getEventTypeCount(filters: EventFeedFilter): Observable<EventTypeCount> {
    const url = `${GATEWAY_URL}/event_type_counts`;

    const options = {
      params: this.buildEventCountsURLSearchParams(filters)
    };

    return this.httpClient
      .get<RespEventCounts>(url, options).pipe(
      map((respCounts) => new EventTypeCount(respCounts)));
  }

  getEventTaskCount(filters: EventFeedFilter): Observable<EventTaskCount> {
    const url = `${GATEWAY_URL}/event_task_counts`;

    const options = {
      params: this.buildEventCountsURLSearchParams(filters)
    };

    return this.httpClient
      .get<RespEventCounts>(url, options).pipe(
      map((respCounts) => {
        return new EventTaskCount(respCounts);
    }));
  }

  getSuggestions(type: string, text: string): Observable<string[]> {
    if (text && text.length > 0) {
      const params = new HttpParams().set('type', type).set('text', text);
      const url = `${CONFIG_MGMT_URL}/suggestions`;

      return this.httpClient.get<Chicklet[]>(url, {params}).pipe(map((suggestions: Chicklet[]) =>
        suggestions.map(item => item.text)));
    } else {
      return observableOf([]);
    }
  }

  getGuitarStrings(filters: EventFeedFilter): Observable<GuitarStringCollection> {
    const url = `${GATEWAY_URL}/eventstrings`;

    const options = {
      params: this.buildEventStringsURLSearchParams(filters)
    };

    return this.httpClient
      .get(url, options).pipe(
      map((res) => this.convertResponseToGuitarStringCollection(res)));
  }

  convertResponseToGuitarStringCollection(json: any): GuitarStringCollection {
    const responseGuitarStringCollection: ResponseGuitarStringCollection = json;
    if (!responseGuitarStringCollection.start ||
      !responseGuitarStringCollection.end ||
      !responseGuitarStringCollection.hours_between) {
      throw new Error('Guitar strings response is missing the start, end, or hours_between value');
    }
    const start = moment(responseGuitarStringCollection.start).startOf('day');
    const end = moment(responseGuitarStringCollection.end).endOf('day');
    const hoursBetween = responseGuitarStringCollection.hours_between;

    if (start.isAfter(end)) {
      throw new Error('Guitar strings response: start date is after end date');
    }

    if ( hoursBetween < 1 || 24 % hoursBetween !== 0) {
      throw new Error('Guitar strings response: hoursBetween is invalid value: ' + hoursBetween );
    }

    const numberOfDays = Math.round(end.diff(start, 'days', true));
    const expectedNumberOfItems = (24 / hoursBetween) * numberOfDays;

    const guitarStringCollection = responseGuitarStringCollection.strings.map(responseString => {
      const eventAction = responseString.event_action;
      const items: GuitarStringItem[] =
        this.createGuitarStringItemCollection(
          start, hoursBetween, responseString.collection, expectedNumberOfItems);

      return new GuitarString(eventAction, items);
    });

    // order the event actions to be 'create', 'delete', and 'update'
    const sortedGuitarStringCollection =
      sortBy(guitarStringCollection, string => string.eventAction);
    return new GuitarStringCollection(sortedGuitarStringCollection, start, end);
  }

  private convertResponseToChefEvents(
    respEventCollection: RespChefEventCollection): ChefEventCollection {
    const events = respEventCollection.events.map(
      (respEvent: RespChefEvent) => new ChefEvent(respEvent));

    return new ChefEventCollection(events, respEventCollection.total_events);
  }

  buildEventCountsURLSearchParams(filters: EventFeedFilter): HttpParams {
    let searchParam: HttpParams = new HttpParams();

    if (filters.searchBar) {
      searchParam = this.flattenSearchBar(filters.searchBar, searchParam);
    }

    if (filters.startDate) {
      searchParam = searchParam.append('start', filters.startDate.valueOf().toString());
    } else {
      searchParam = searchParam.append('start',
        initialState.filters.startDate.valueOf().toString());
    }

    if (filters.endDate) {
      searchParam = searchParam.append('end', filters.endDate.valueOf().toString());
    }

    return searchParam;
  }

  buildEventStringsURLSearchParams(filters: EventFeedFilter): HttpParams {
    let searchParam: HttpParams = new HttpParams();

    if (filters.searchBar) {
      searchParam = this.flattenSearchBar(filters.searchBar, searchParam);
    }

    searchParam = searchParam.append('timezone', Intl.DateTimeFormat().resolvedOptions().timeZone);

    if (filters.hoursBetween) {
      searchParam = searchParam.append('hours_between', filters.hoursBetween.toString());
    } else {
      searchParam = searchParam.append('hours_between', '1');
    }

    if (filters.startDate) {
      searchParam = searchParam.append('start', filters.startDate.format('YYYY-MM-DD'));
    } else {
      searchParam = searchParam.append('start',
        initialState.filters.startDate.format('YYYY-MM-DD'));
    }

    if (filters.endDate) {
      searchParam = searchParam.append('end', filters.endDate.format('YYYY-MM-DD'));
    } else {
      searchParam = searchParam.append('end', moment().format('YYYY-MM-DD'));
    }

    return searchParam;
  }

  private flattenSearchBar(filters: Chicklet[], searchParam: HttpParams): HttpParams {
    return reduce((params: HttpParams, filter: { type: string, text: string }) => {
      const filterParam = `${encodeURIComponent(filter.type)}:${encodeURIComponent(filter.text)}`;
      return params.append('filter', filterParam);
    }, searchParam, filters);
  }

  addChildEventTypes(filters: Chicklet[]): Chicklet[] {
    const entityTypes: string[] = filters.filter((filter: Chicklet) =>
      filter.type === ENTITY_TYPE_TAG).map((filter: Chicklet) => filter.text);

    if (includes(ENTITY_TYPE_DATA_BAG_TAG, entityTypes) &&
      !includes(ENTITY_TYPE_DATA_BAG_ITEM_TAG, entityTypes)) {
      filters = concat(filters, {type: ENTITY_TYPE_TAG, text: ENTITY_TYPE_DATA_BAG_ITEM_TAG});
    }

    if (includes(ENTITY_TYPE_COOKBOOK_TAG, entityTypes) &&
      !includes(ENTITY_TYPE_COOKBOOK_VERSION_TAG, entityTypes)) {
      filters = concat(filters, {type: ENTITY_TYPE_TAG, text: ENTITY_TYPE_COOKBOOK_VERSION_TAG});
    }

    return filters;
  }

  buildEventFeedURLSearchParams(filters: EventFeedFilter,
    lastEvent: ChefEvent): HttpParams {
    let searchParam: HttpParams = new HttpParams();

    // By default, we want to collapse events of the same type, action and
    // performed by the same user
    if (filters.collapse === false) {
      searchParam = searchParam.append('collapse', 'false');
    } else {
      searchParam = searchParam.append('collapse', 'true');
    }

    if (filters.searchBar) {
      const searchBarWithChildren = this.addChildEventTypes(filters.searchBar);
      searchParam = this.flattenSearchBar(searchBarWithChildren, searchParam);
    }

    if (filters.task) {
      searchParam = searchParam.append('filter', `task:${filters.task}`);
    }

    if (filters.requestorName) {
      searchParam = searchParam.append('filter', `requestor_name:${filters.requestorName}`);
    }

    if (filters.pageSize) {
      searchParam = searchParam.append('page_size', `${filters.pageSize}`);
    } else {
      searchParam = searchParam.append('page_size', '100');
    }

    if (filters.startDate) {
      searchParam = searchParam.append('start', filters.startDate.valueOf().toString());
    } else {
      searchParam = searchParam.append('start',
        initialState.filters.startDate.valueOf().toString());
    }

    if (filters.endDate) {
      searchParam = searchParam.append('end', filters.endDate.valueOf().toString());
    }

    if (lastEvent) {
      searchParam = searchParam.append('before', new Date(lastEvent.endTime).valueOf().toString());
      searchParam = searchParam.append('cursor', lastEvent.endId);
    }

    return searchParam;
  }

  private createGuitarStringItemCollection(
    initialStart: moment.Moment,
    hoursBetweenItems: number,
    respEventCollection: RespEventCollection[],
    expectedNumberOfItems: number): GuitarStringItem[] {

    const guitarStringItemCollection =
      respEventCollection.map((eventCollection: RespEventCollection, index: number) => {
      const start = initialStart.clone().add(
        hoursBetweenItems * index, 'hours').startOf('hour');
      const end = initialStart.clone().add(
        hoursBetweenItems * index, 'hours').endOf('hour');
      return new GuitarStringItem(eventCollection.events_count, start, end);
    });

    // We are guaranteeing that each day has 24 hours
    // This solves the when daylight saving time start or end is within the range.
    if ( guitarStringItemCollection.length > expectedNumberOfItems ) {
      return guitarStringItemCollection.slice(0, expectedNumberOfItems);
    } else if ( guitarStringItemCollection.length < expectedNumberOfItems ) {
      for ( let index = guitarStringItemCollection.length; index < expectedNumberOfItems; index++) {
        const start = initialStart.clone().add(
          hoursBetweenItems * index, 'hours').startOf('hour');
        const end = initialStart.clone().add(
          hoursBetweenItems * index, 'hours').endOf('hour');

        guitarStringItemCollection.push(new GuitarStringItem([], start, end));
      }
    }

    return guitarStringItemCollection;
  }
}
