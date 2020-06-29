import { Action } from '@ngrx/store';
import { HttpErrorResponse } from '@angular/common/http';

import { DailyCheckInCountCollection, NodeRunsDailyStatusCollection,
  TopErrorsCollection, CountedDurationCollection, Desktop, TermFilter,
  PageSizeChangeEvent } from './desktop.model';

export enum DesktopActionTypes {
  GET_DAILY_CHECK_IN_TIME_SERIES                     = 'DESKTOP::GET::DAILY_CHECK_IN_TIME_SERIES',
  GET_DAILY_CHECK_IN_TIME_SERIES_SUCCESS             = 'DESKTOP::GET::DAILY_CHECK_IN_TIME_SERIES::SUCCESS',
  GET_DAILY_CHECK_IN_TIME_SERIES_FAILURE             = 'DESKTOP::GET::DAILY_CHECK_IN_TIME_SERIES::FAILURE',
  GET_DAILY_NODE_RUNS_STATUS_TIME_SERIES             = 'DESKTOP::GET::DAILY_NODE_RUNS_STATUS_TIME_SERIES',
  GET_DAILY_NODE_RUNS_STATUS_TIME_SERIES_SUCCESS     = 'DESKTOP::GET::DAILY_NODE_RUNS_STATUS_TIME_SERIES::SUCCESS',
  GET_DAILY_NODE_RUNS_STATUS_TIME_SERIES_FAILURE     = 'DESKTOP::GET::DAILY_NODE_RUNS_STATUS_TIME_SERIES::FAILURE',
  GET_TOP_ERRORS_COLLECTION                          = 'DESKTOP::GET::TOP_ERRORS_COLLECTION',
  GET_TOP_ERRORS_COLLECTION_SUCCESS                  = 'DESKTOP::GET::TOP_ERRORS_COLLECTION::SUCCESS',
  GET_TOP_ERRORS_COLLECTION_FAILURE                  = 'DESKTOP::GET::TOP_ERRORS_COLLECTION::FAILURE',
  GET_UNKNOWN_DESKTOP_DURATION_COUNTS                = 'DESKTOP::GET::UNKNOWN_DESKTOP_DURATION_COUNT',
  GET_UNKNOWN_DESKTOP_DURATION_COUNTS_SUCCESS        = 'DESKTOP::GET::UNKNOWN_DESKTOP_DURATION_COUNT::SUCCESS',
  GET_UNKNOWN_DESKTOP_DURATION_COUNTS_FAILURE        = 'DESKTOP::GET::UNKNOWN_DESKTOP_DURATION_COUNT::FAILURE',
  GET_DESKTOPS                                       = 'DESKTOP::GET::DESKTOPS',
  GET_DESKTOPS_SUCCESS                               = 'DESKTOP::GET::DESKTOPS::SUCCESS',
  GET_DESKTOPS_FAILURE                               = 'DESKTOP::GET::DESKTOPS::FAILURE',
  GET_DESKTOPS_TOTAL                                 = 'DESKTOP::GET::DESKTOPS_TOTAL',
  GET_DESKTOPS_TOTAL_SUCCESS                         = 'DESKTOP::GET::DESKTOPS_TOTAL::SUCCESS',
  GET_DESKTOPS_TOTAL_FAILURE                         = 'DESKTOP::GET::DESKTOPS_TOTAL::FAILURE',
  SET_SELECTED_DESKTOP                               = 'DESKTOP::SET::DESKTOP',
  SET_SELECTED_DAYS_AGO                              = 'DESKTOP::SET::DAYS_AGO',
  UPDATE_DESKTOPS_FILTER_CURRENT_PAGE                = 'DESKTOP::UPDATE::DESKTOPS_FILTER_CURRENT_PAGE',
  ADD_DESKTOPS_FILTER_TERM                           = 'DESKTOP::ADD::DESKTOPS_FILTER_TERM',
  UPDATE_DESKTOPS_FILTER_TERMS                       = 'DESKTOP::UPDATE::DESKTOPS_FILTER_TERMS',
  REMOVE_DESKTOPS_FILTER_TERM                        = 'DESKTOP::REMOVE::DESKTOPS_FILTER_TERM',
  UPDATE_DESKTOPS_SORT_TERM                          = 'DESKTOP::UPDATE::DESKTOPS_SORT_TERM',
  UPDATE_DESKTOPS_FILTER_PAGE_SIZE_AND_CURRENT_PAGE  = 'DESKTOP::UPDATE::DESKTOPS_FILTER_PAGE_SIZE_AND_CURRENT_PAGE'
}

export class SetSelectedDesktop implements Action {
  readonly type = DesktopActionTypes.SET_SELECTED_DESKTOP;
  constructor(public payload: {desktop: Desktop}) { }
}

export class SetSelectedDaysAgo implements Action {
  readonly type = DesktopActionTypes.SET_SELECTED_DAYS_AGO;
  constructor(public payload: {daysAgo: number}) { }
}

export class GetDailyCheckInTimeSeries implements Action {
  readonly type = DesktopActionTypes.GET_DAILY_CHECK_IN_TIME_SERIES;
}

export class GetDailyCheckInTimeSeriesSuccess implements Action {
  readonly type = DesktopActionTypes.GET_DAILY_CHECK_IN_TIME_SERIES_SUCCESS;
  constructor(public payload: DailyCheckInCountCollection) { }
}

export class GetDailyCheckInTimeSeriesFailure implements Action {
  readonly type = DesktopActionTypes.GET_DAILY_CHECK_IN_TIME_SERIES_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class GetDailyNodeRunsStatusTimeSeries implements Action {
  readonly type = DesktopActionTypes.GET_DAILY_NODE_RUNS_STATUS_TIME_SERIES;
  constructor(public nodeId: string, public daysAgo: number ) { }
}

export class GetDailyNodeRunsStatusTimeSeriesSuccess implements Action {
  readonly type = DesktopActionTypes.GET_DAILY_NODE_RUNS_STATUS_TIME_SERIES_SUCCESS;
  constructor(public payload: NodeRunsDailyStatusCollection) { }
}

export class GetDailyNodeRunsStatusTimeSeriesFailure implements Action {
  readonly type = DesktopActionTypes.GET_DAILY_NODE_RUNS_STATUS_TIME_SERIES_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class GetTopErrorsCollection implements Action {
  readonly type = DesktopActionTypes.GET_TOP_ERRORS_COLLECTION;
}

export class GetTopErrorsCollectionSuccess implements Action {
  readonly type = DesktopActionTypes.GET_TOP_ERRORS_COLLECTION_SUCCESS;
  constructor(public payload: TopErrorsCollection) { }
}

export class GetTopErrorsCollectionFailure implements Action {
  readonly type = DesktopActionTypes.GET_TOP_ERRORS_COLLECTION_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class GetUnknownDesktopDurationCounts implements Action {
  readonly type = DesktopActionTypes.GET_UNKNOWN_DESKTOP_DURATION_COUNTS;
}

export class GetUnknownDesktopDurationCountsSuccess implements Action {
  readonly type = DesktopActionTypes.GET_UNKNOWN_DESKTOP_DURATION_COUNTS_SUCCESS;
  constructor(public payload: CountedDurationCollection) { }
}

export class GetUnknownDesktopDurationCountsFailure implements Action {
  readonly type = DesktopActionTypes.GET_UNKNOWN_DESKTOP_DURATION_COUNTS_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class GetDesktops implements Action {
  readonly type = DesktopActionTypes.GET_DESKTOPS;
}

export class GetDesktopsSuccess implements Action {
  readonly type = DesktopActionTypes.GET_DESKTOPS_SUCCESS;
  constructor(public payload: Desktop[]) { }
}

export class GetDesktopsFailure implements Action {
  readonly type = DesktopActionTypes.GET_DESKTOPS_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class GetDesktopsTotal implements Action {
  readonly type = DesktopActionTypes.GET_DESKTOPS_TOTAL;
}

export class GetDesktopsTotalSuccess implements Action {
  readonly type = DesktopActionTypes.GET_DESKTOPS_TOTAL_SUCCESS;
  constructor(public payload: number) { }
}

export class GetDesktopsTotalFailure implements Action {
  readonly type = DesktopActionTypes.GET_DESKTOPS_TOTAL_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class UpdateDesktopFilterCurrentPage implements Action {
  readonly type = DesktopActionTypes.UPDATE_DESKTOPS_FILTER_CURRENT_PAGE;
  constructor(public payload: { page: number}) { }
}

export class AddDesktopFilterTerm implements Action {
  readonly type = DesktopActionTypes.ADD_DESKTOPS_FILTER_TERM;
  constructor(public payload: { term: TermFilter}) { }
}

export class UpdateDesktopFilterTerm implements Action {
  readonly type = DesktopActionTypes.UPDATE_DESKTOPS_FILTER_TERMS;
  constructor(public payload: { terms: TermFilter[]}) { }
}

export class RemoveDesktopFilterTerm implements Action {
  readonly type = DesktopActionTypes.REMOVE_DESKTOPS_FILTER_TERM;
  constructor(public payload: { term: TermFilter}) { }
}

export class UpdateDesktopSortTerm implements Action {
  readonly type = DesktopActionTypes.UPDATE_DESKTOPS_SORT_TERM;
  constructor(public payload: { term: string }) { }
}

export class UpdateDesktopsFilterPageSizeAndCurrentPage implements Action {
  readonly type = DesktopActionTypes.UPDATE_DESKTOPS_FILTER_PAGE_SIZE_AND_CURRENT_PAGE;
  constructor(public payload: PageSizeChangeEvent) { }
}

export type DesktopActions =
  | SetSelectedDaysAgo
  | SetSelectedDesktop
  | GetDailyCheckInTimeSeries
  | GetDailyCheckInTimeSeriesSuccess
  | GetDailyCheckInTimeSeriesFailure
  | GetDailyNodeRunsStatusTimeSeries
  | GetDailyNodeRunsStatusTimeSeriesSuccess
  | GetDailyNodeRunsStatusTimeSeriesFailure
  | GetTopErrorsCollection
  | GetTopErrorsCollectionSuccess
  | GetTopErrorsCollectionFailure
  | GetUnknownDesktopDurationCounts
  | GetUnknownDesktopDurationCountsSuccess
  | GetUnknownDesktopDurationCountsFailure
  | GetDesktops
  | GetDesktopsSuccess
  | GetDesktopsFailure
  | GetDesktopsTotal
  | GetDesktopsTotalSuccess
  | GetDesktopsTotalFailure
  | UpdateDesktopFilterCurrentPage
  | AddDesktopFilterTerm
  | UpdateDesktopFilterTerm
  | RemoveDesktopFilterTerm
  | UpdateDesktopSortTerm
  | UpdateDesktopsFilterPageSizeAndCurrentPage;
