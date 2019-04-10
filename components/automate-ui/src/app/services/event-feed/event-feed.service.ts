import { map } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { Observable } from 'rxjs';

import { reduce } from 'lodash/fp';
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
  SidebarFilter
} from '../../types/types';
import { environment } from '../../../environments/environment';
import * as moment from 'moment';
const GATEWAY_URL = environment.gateway_url;

@Injectable()
export class EventFeedService {
  constructor(
    private httpClient: HttpClient
  ) {}

  getEventFeed(filters: EventFeedFilter,
    sidebarFilter: SidebarFilter): Observable<ChefEventCollection> {
    const url = `${GATEWAY_URL}/eventfeed`;

    const options = {
      params: this.buildEventFeedURLSearchParams(filters, sidebarFilter, null)
    };

    return this.httpClient
      .get<RespChefEventCollection>(url, options).pipe(
      map((res) => this.convertResponseToChefEvents(res)));
  }

  loadMoreEventFeed(filters: EventFeedFilter, sidebarFilter: SidebarFilter,
    lastEvent: ChefEvent): Observable<ChefEventCollection> {
    const url = `${GATEWAY_URL}/eventfeed`;

    const options = {
      params: this.buildEventFeedURLSearchParams(filters, sidebarFilter, lastEvent)
    };

    return this.httpClient
      .get<RespChefEventCollection>(url, options).pipe(
      map((res) => this.convertResponseToChefEvents(res)));
  }

  getEventTypeCount(filters: EventFeedFilter,
    sidebarFilter: SidebarFilter): Observable<EventTypeCount> {
    const url = `${GATEWAY_URL}/event_type_counts`;

    const options = {
      params: this.buildEventTypeCountsURLSearchParams(filters, sidebarFilter)
    };

    return this.httpClient
      .get<RespEventCounts>(url, options).pipe(
      map((respCounts) => new EventTypeCount(respCounts)));
  }

  getEventTaskCount(filters: EventFeedFilter,
    sidebarFilter: SidebarFilter): Observable<EventTaskCount> {
    const url = `${GATEWAY_URL}/event_task_counts`;

    const options = {
      params: this.buildEventTaskCountsURLSearchParams(filters, sidebarFilter)
    };

    return this.httpClient
      .get<RespEventCounts>(url, options).pipe(
      map((respCounts) => {
        return new EventTaskCount(respCounts);
    }));
  }

  getGuitarStrings(filters: EventFeedFilter,
    sidebarFilter: SidebarFilter): Observable<GuitarStringCollection> {
    const url = `${GATEWAY_URL}/eventstrings`;

    const options = {
      params: this.buildEventStringsURLSearchParams(filters, sidebarFilter)
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

  private buildEventTaskCountsURLSearchParams(filters: EventFeedFilter,
    sidebarFilter: SidebarFilter): HttpParams {
    let searchParam: HttpParams = new HttpParams();

    if (sidebarFilter.servers) {
      searchParam = reduce((param, server) => param.append('filter', `source_fqdn:${server}`),
                           searchParam,
                           sidebarFilter.servers);
    }

    if (sidebarFilter.organizations) {
      searchParam = reduce((param, org) => param.append('filter', `organization:${org}`),
                           searchParam,
                           sidebarFilter.organizations);
    }

    if (filters.entityType) {
      if (filters.entityType instanceof Array) {
        searchParam = reduce((param, eType) => param.append('filter', `entity_type:${eType}`),
                             searchParam,
                             filters.entityType);
      } else {
        searchParam = searchParam.append('filter', `entity_type:${filters.entityType}`);
      }
    }

    if (filters.startDate) {
      searchParam = searchParam.append('start', filters.startDate.valueOf().toString());
    } else {
      searchParam = searchParam.append('start',
      moment().subtract(6, 'days').startOf('day').valueOf().toString());
    }

    if (filters.endDate) {
      searchParam = searchParam.append('end', filters.endDate.valueOf().toString());
    }

    return searchParam;
  }

  private buildEventTypeCountsURLSearchParams(filters: EventFeedFilter,
    sidebarFilter: SidebarFilter): HttpParams {
    let searchParam: HttpParams = new HttpParams();

    if (sidebarFilter.servers) {
      searchParam = reduce((param, server) => param.append('filter', `source_fqdn:${server}`),
                           searchParam,
                           sidebarFilter.servers);
    }

    if (sidebarFilter.organizations) {
      searchParam = reduce((param, org) => param.append('filter', `organization:${org}`),
                           searchParam,
                           sidebarFilter.organizations);
    }

    if (filters.startDate) {
      searchParam = searchParam.append('start', filters.startDate.valueOf().toString());
    } else {
      searchParam = searchParam.append('start',
                                       moment()
                                       .subtract(6, 'days')
                                       .startOf('day')
                                       .valueOf()
                                       .toString());
    }

    if (filters.endDate) {
      searchParam = searchParam.append('end', filters.endDate.valueOf().toString());
    }

    return searchParam;
  }

  private buildEventStringsURLSearchParams(filters: EventFeedFilter,
    sidebarFilter: SidebarFilter): HttpParams {
    let searchParam: HttpParams = new HttpParams();

    if (sidebarFilter.servers) {
      searchParam = reduce((param, server) => param.append('filter', `source_fqdn:${server}`),
                           searchParam,
                           sidebarFilter.servers);
    }

    if (sidebarFilter.organizations) {
      searchParam = reduce((param, org) => param.append('filter', `organization:${org}`),
                           searchParam,
                           sidebarFilter.organizations);
    }

    if (filters.entityType) {
      if (filters.entityType instanceof Array) {
        searchParam = reduce((param, eType) => param.append('filter', `entity_type:${eType}`),
                             searchParam,
                             filters.entityType);
      } else {
        searchParam = searchParam.append('filter', `entity_type:${filters.entityType}`);
      }
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
                                       moment()
                                       .subtract(6, 'days')
                                       .startOf('day')
                                       .format('YYYY-MM-DD'));
    }

    if (filters.endDate) {
      searchParam = searchParam.append('end', filters.endDate.format('YYYY-MM-DD'));
    } else {
      searchParam = searchParam.append('end', moment().format('YYYY-MM-DD'));
    }

    return searchParam;
  }

  private buildEventFeedURLSearchParams(filters: EventFeedFilter,
    sidebarFilter: SidebarFilter, lastEvent: ChefEvent): HttpParams {
    let searchParam: HttpParams = new HttpParams();

    // By default, we want to collapse events of the same type, action and
    // performed by the same user
    if (filters.collapse === false) {
      searchParam = searchParam.append('collapse', 'false');
    } else {
      searchParam = searchParam.append('collapse', 'true');
    }

    if (sidebarFilter.servers) {
      searchParam = reduce((params, server) => params.append('filter', `source_fqdn:${server}`),
                           searchParam,
                           sidebarFilter.servers);
    }

    if (sidebarFilter.organizations) {
      searchParam = reduce((params, org) => params.append('filter', `organization:${org}`),
                           searchParam,
                           sidebarFilter.organizations);
    }

    if (filters.entityType) {
      if (filters.entityType instanceof Array) {
        searchParam = reduce((params, eType) => params.append('filter', `entity_type:${eType}`),
                             searchParam,
                             filters.entityType);
      } else {
        searchParam = searchParam.append('filter', `entity_type:${filters.entityType}`);
      }
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
                                       moment()
                                       .subtract(6, 'days')
                                       .startOf('day')
                                       .valueOf()
                                       .toString());
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
