import { set, pipe } from 'lodash/fp';

import { EntityStatus } from '../entities';
import { DesktopActionTypes, DesktopActions } from './desktop.actions';
import { DailyCheckInCountCollection } from './desktop.model';

export interface DesktopEntityState {
  dailyCheckInCountCollection: DailyCheckInCountCollection;
  getDailyCheckInTimeSeriesStatus: EntityStatus;
}

export const desktopEntityInitialState: DesktopEntityState = {
  dailyCheckInCountCollection: { buckets: []},
  getDailyCheckInTimeSeriesStatus: EntityStatus.notLoaded
};

export function userEntityReducer(state: DesktopEntityState = desktopEntityInitialState,
  action: DesktopActions) {

  switch (action.type) {
    case DesktopActionTypes.GET_DAILY_CHECK_IN_TIME_SERIES:
      return set('getDailyCheckInTimeSeriesStatus', EntityStatus.loading, state);

    case DesktopActionTypes.GET_DAILY_CHECK_IN_TIME_SERIES_SUCCESS:
      return pipe(
        set('getDailyCheckInTimeSeriesStatus', EntityStatus.loadingSuccess),
        set('dailyCheckInCountCollection', action.payload))(state);

    case DesktopActionTypes.GET_DAILY_CHECK_IN_TIME_SERIES_FAILURE:
      return set('getDailyCheckInTimeSeriesStatus', EntityStatus.loadingFailure, state);

    default:
      return state;

  }
}
