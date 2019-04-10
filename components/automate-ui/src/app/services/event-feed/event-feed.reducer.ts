import { EventFeedFilter,
  GuitarStringCollection,
  EventTypeCount,
  EventTaskCount,
  ChefEvent } from '../../types/types';
import {
  GET_INITIAL_FEED,
  GET_INITIAL_FEED_SUCCESS,
  GET_INITIAL_FEED_ERROR,
  GET_INITIAL_FEED_PERMISSION_DENIED,
  GET_GUITAR_STRINGS,
  GET_GUITAR_STRINGS_SUCCESS,
  GET_GUITAR_STRINGS_ERROR,
  GET_TYPE_COUNTS,
  GET_TYPE_COUNTS_SUCCESS,
  GET_TYPE_COUNTS_ERROR,
  ADD_FEED_FILTER,
  LOAD_MORE_FEED,
  LOAD_MORE_FEED_ERROR,
  LOAD_MORE_FEED_SUCCESS,
  GET_TASK_COUNTS,
  GET_TASK_COUNTS_SUCCESS,
  GET_TASK_COUNTS_ERROR,
  ADD_FEED_DATE_RANGE_FILTER,
  GET_INITIAL_EVENT_FEED_LOAD,
  EventFeedAction } from './event-feed.actions';
import * as moment from 'moment';

import {
 set,
 pipe
} from 'lodash/fp';

export enum Status {
  notLoaded,
  loading,
  loadingSuccess,
  loadingFailure,
  loadingPermissionDenied
}

export interface EventFeedState {
  completeNumberOfEvents: number;
  loadedEvents: ChefEvent[];
  guitarStringCollection: GuitarStringCollection;
  typeCounts: EventTypeCount;
  taskCounts: EventTaskCount;
  filters: EventFeedFilter;
  initialFeedStatus: Status;
  loadMorefeedStatus: Status;
  guitarStringsStatus: Status;
  eventTypeCountStatus: Status;
  eventTaskCountStatus: Status;
}

export const initialState: EventFeedState = {
  completeNumberOfEvents: 0,
  loadedEvents: [],
  guitarStringCollection:
    new GuitarStringCollection([],
    moment().subtract(6, 'days').startOf('day'),
    moment().endOf('day')),
  typeCounts: EventTypeCount.Null,
  taskCounts: EventTaskCount.Null,
  filters: {
    entityType: [],
    endDate: moment().endOf('day'),
    startDate: moment().subtract(6, 'days').startOf('day'),
    hoursBetween: 1,
    pageSize: 100,
    collapse: true
  },
  loadMorefeedStatus: Status.loadingSuccess,
  initialFeedStatus: Status.notLoaded,
  guitarStringsStatus: Status.notLoaded,
  eventTypeCountStatus: Status.notLoaded,
  eventTaskCountStatus: Status.notLoaded
};

export function eventFeedReducer(
  state: EventFeedState = initialState,
  action: EventFeedAction): EventFeedState {

  switch (action.type) {

    // When loading the page initialize the state
    case GET_INITIAL_EVENT_FEED_LOAD: {
      return pipe(
        set('completeNumberOfEvents', initialState.completeNumberOfEvents),
        set('typeCounts', initialState.typeCounts),
        set('taskCounts', initialState.taskCounts),
        set('loadedEvents', initialState.loadedEvents),
        set('guitarStringCollection', initialState.guitarStringCollection),
        set('filters', initialState.filters),
        set('initialFeedStatus', initialState.initialFeedStatus),
        set('guitarStringsStatus', initialState.guitarStringsStatus),
        set('eventTypeCountStatus', initialState.eventTypeCountStatus),
        set('eventTaskCountStatus', initialState.eventTaskCountStatus),
        set('loadMorefeedStatus', initialState.loadMorefeedStatus)
      )(state) as EventFeedState;
    }

    // We only want the totalEvents events from the initial request, because
    // later requests could have more event added to the front of the list.
    // This would change the total count, but not the total to load on the page
    case GET_INITIAL_FEED: {
      return pipe(
        set('initialFeedStatus', Status.loading),
        set('completeNumberOfEvents', 0),
        set('loadedEvents', [])
      )(state) as EventFeedState;
    }

    case GET_INITIAL_FEED_SUCCESS: {
      return pipe(set('loadedEvents', action.payload.events),
        set('completeNumberOfEvents', action.payload.totalEvents),
        set('initialFeedStatus', Status.loadingSuccess)
      )(state) as EventFeedState;
    }

    case GET_INITIAL_FEED_ERROR: {
      return set('initialFeedStatus', Status.loadingFailure, state) as EventFeedState;
    }

    case GET_INITIAL_FEED_PERMISSION_DENIED: {
      return set('initialFeedStatus', Status.loadingPermissionDenied, state) as EventFeedState;
    }

    case LOAD_MORE_FEED: {
      return set('feedStatus', Status.loading, state) as EventFeedState;
    }

    case LOAD_MORE_FEED_SUCCESS: {
      return pipe(
        set('loadedEvents', state.loadedEvents.concat(action.payload.events)),
        set('loadMorefeedStatus', Status.loadingSuccess)
      )(state) as EventFeedState;
    }

    case LOAD_MORE_FEED_ERROR: {
      return set('loadMorefeedStatus', Status.loadingFailure, state);
    }

    case GET_GUITAR_STRINGS: {
      return set('guitarStringsStatus', Status.loading, state);
    }

    case GET_GUITAR_STRINGS_SUCCESS: {
      return pipe(
        set('guitarStringCollection', action.payload),
        set('guitarStringsStatus', Status.loadingSuccess)
      )(state) as EventFeedState;
    }

    case GET_GUITAR_STRINGS_ERROR: {
      return set('guitarStringsStatus', Status.loadingFailure, state) as EventFeedState;
    }

    case GET_TYPE_COUNTS: {
      return set('eventTypeCountStatus', Status.loading, state) as EventFeedState;
    }

    case GET_TYPE_COUNTS_SUCCESS: {
      return pipe(
        set('typeCounts', action.payload),
        set('eventTypeCountStatus', Status.loadingSuccess)
      )(state) as EventFeedState;
    }

    case GET_TYPE_COUNTS_ERROR: {
      return set('eventTypeCountStatus', Status.loadingFailure, state) as EventFeedState;
    }

    case GET_TASK_COUNTS: {
      return set('eventTaskCountStatus', Status.loading, state) as EventFeedState;
    }

    case GET_TASK_COUNTS_SUCCESS: {
      return pipe(
        set('taskCounts', action.payload),
        set('eventTaskCountStatus', Status.loadingSuccess)
      )(state) as EventFeedState;
    }

    case GET_TASK_COUNTS_ERROR: {
      return set('eventTaskCountStatus', Status.loadingFailure, state) as EventFeedState;
    }

    case ADD_FEED_FILTER: {
      const {type, filter} = action.payload;
      return set('filters.' + type, filter, state) as EventFeedState;
    }

    case ADD_FEED_DATE_RANGE_FILTER: {
      const {start, end} = action.payload;
      return pipe(
        set('filters.startDate', start),
        set('filters.endDate', end)
      )(state) as EventFeedState;
    }
  }

  return state;
}
