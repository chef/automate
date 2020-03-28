import { set, pipe } from 'lodash/fp';

import { EntityStatus } from '../entities';
import { DesktopActionTypes, DesktopActions } from './desktop.actions';
import { DailyCheckInCountCollection, TopErrorsCollection } from './desktop.model';

export interface DesktopEntityState {
  dailyCheckInCountCollection: DailyCheckInCountCollection;
  getDailyCheckInTimeSeriesStatus: EntityStatus;
  selectedDaysAgo: number;
  topErrorCollection: TopErrorsCollection;
  getTopErrorCollectionStatus: EntityStatus;
}

export const desktopEntityInitialState: DesktopEntityState = {
  dailyCheckInCountCollection: { buckets: []},
  getDailyCheckInTimeSeriesStatus: EntityStatus.notLoaded,
  selectedDaysAgo: 3,
  topErrorCollection: {items: []},
  getTopErrorCollectionStatus: EntityStatus.notLoaded
};

export function userEntityReducer(state: DesktopEntityState = desktopEntityInitialState,
  action: DesktopActions) {

  switch (action.type) {
    case DesktopActionTypes.SET_DAYS_AGO_SELECTED:
      return set('selectedDaysAgo', action.payload.daysAgo, state);

    case DesktopActionTypes.GET_DAILY_CHECK_IN_TIME_SERIES:
      return set('getDailyCheckInTimeSeriesStatus', EntityStatus.loading, state);

    case DesktopActionTypes.GET_DAILY_CHECK_IN_TIME_SERIES_SUCCESS:
      return pipe(
        set('getDailyCheckInTimeSeriesStatus', EntityStatus.loadingSuccess),
        set('dailyCheckInCountCollection', action.payload))(state);

    case DesktopActionTypes.GET_DAILY_CHECK_IN_TIME_SERIES_FAILURE:
      return set('getDailyCheckInTimeSeriesStatus', EntityStatus.loadingFailure, state);

    case DesktopActionTypes.GET_TOP_ERRORS_COLLECTION:
      return set('getTopErrorCollectionStatus', EntityStatus.loading, state);

    case DesktopActionTypes.GET_TOP_ERRORS_COLLECTION_SUCCESS:
      return pipe(
        set('getTopErrorCollectionStatus', EntityStatus.loadingSuccess),
        set('topErrorCollection', action.payload))(state);

    case DesktopActionTypes.GET_TOP_ERRORS_COLLECTION_FAILURE:
      return set('getTopErrorCollectionStatus', EntityStatus.loadingFailure, state);

    default:
      return state;

  }
}
