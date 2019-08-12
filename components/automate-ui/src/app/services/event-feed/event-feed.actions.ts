import { Action } from '@ngrx/store';

export interface EventFeedAction extends Action {
  payload: any;
}

export const GET_GUITAR_STRINGS = 'GET::GUITAR::STRINGS';
export const getGuitarStrings =
  (payload = {}): EventFeedAction => ({ type: GET_GUITAR_STRINGS, payload });

export const GET_GUITAR_STRINGS_SUCCESS = 'GET::GUITAR::STRINGS::SUCCESS';
export const getGuitarStringsSuccess = (payload): EventFeedAction =>
  ({ type: GET_GUITAR_STRINGS_SUCCESS, payload });

export const GET_GUITAR_STRINGS_ERROR = 'GET::GUITAR::STRINGS::ERROR';

export const GET_INITIAL_EVENT_FEED_LOAD = 'GET::INITIAL::EVENT::FEED::LOAD';
export const getInitialEventFeedLoad = (payload = {}): EventFeedAction =>
  ({ type: GET_INITIAL_EVENT_FEED_LOAD, payload });

export const GET_INITIAL_FEED = 'GET::INITIAL::FEED';
export const getInitialFeed = (payload = {}): EventFeedAction =>
  ({ type: GET_INITIAL_FEED, payload });

export const GET_INITIAL_FEED_SUCCESS = 'GET::INITIAL::FEED::SUCCESS';
export const getInitialFeedSuccess = (payload): EventFeedAction =>
  ({ type: GET_INITIAL_FEED_SUCCESS, payload });

export const GET_INITIAL_FEED_ERROR = 'GET::FEED::ERROR';

export const GET_INITIAL_FEED_PERMISSION_DENIED = 'GET::FEED::DENIED';

export const LOAD_MORE_FEED = 'LOAD::MORE::FEED';
export const loadMoreFeed = (payload = {}): EventFeedAction =>
  ({ type: LOAD_MORE_FEED, payload });

export const LOAD_MORE_FEED_SUCCESS = 'LOAD::MORE::FEED::SUCCESS';
export const loadMoreFeedSuccess = (payload): EventFeedAction =>
  ({ type: LOAD_MORE_FEED_SUCCESS, payload });

export const LOAD_MORE_FEED_ERROR = 'LOAD::MORE::FEED::ERROR';

export const GET_TYPE_COUNTS = 'GET::TYPE::COUNTS';
export const getTypeCounts =
  (payload = {}): EventFeedAction => ({ type: GET_TYPE_COUNTS, payload });

export const GET_TYPE_COUNTS_SUCCESS = 'GET::TYPE::COUNTS::SUCCESS';
export const getTypeCountsSuccess = (payload): EventFeedAction =>
  ({ type: GET_TYPE_COUNTS_SUCCESS, payload });

export const GET_TYPE_COUNTS_ERROR = 'GET::TYPE::COUNTS::ERROR';

export const GET_TASK_COUNTS = 'GET::TASK::COUNTS';
export const getTaskCounts =
  (payload = {}): EventFeedAction => ({ type: GET_TASK_COUNTS, payload });

export const GET_TASK_COUNTS_SUCCESS = 'GET::TASK::COUNTS::SUCCESS';
export const getTaskCountsSuccess = (payload): EventFeedAction =>
  ({ type: GET_TASK_COUNTS_SUCCESS, payload });

export const GET_TASK_COUNTS_ERROR = 'GET::TASK::COUNTS::ERROR';

export const ADD_SEARCH_BAR_FILTERS = 'ADD::SEARCH::BAR::FILTERS';
export const addSearchbarFilters = (searchBarFilters): EventFeedAction =>
  ({ type: ADD_SEARCH_BAR_FILTERS, payload: {searchBarFilters} });

export const ADD_FEED_DATE_RANGE_FILTER = 'ADD::FEED::DATE:RANGE::FILTER';
export const addFeedDateRangeFilter = (start, end): EventFeedAction =>
  ({ type: ADD_FEED_DATE_RANGE_FILTER, payload: {start, end} });

export const GET_SUGGESTIONS = 'GET::SUGGESTIONS';
export const getSuggestions = (type, text): EventFeedAction =>
  ({ type: GET_SUGGESTIONS, payload: {type, text} });

export const GET_SUGGESTIONS_SUCCESS = 'GET::SUGGESTIONS::SUCCESS';
export const getSuggestionsSuccess = (payload): EventFeedAction =>
  ({ type: GET_SUGGESTIONS_SUCCESS, payload });

export const GET_SUGGESTIONS_ERROR  = 'GET::SUGGESTIONS::ERROR';
