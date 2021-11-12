import { HttpErrorResponse } from '@angular/common/http';
import { set } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { DownloadReportsActionTypes, DownloadReportsActions } from './download-reports.actions';

export interface DownloadReportsEntityState {
  list: {
    notificationStack: string[]
  };
  error: HttpErrorResponse;
  status: EntityStatus;
}

export const DownloadReportsEntityInitialState: DownloadReportsEntityState = {
  list: {
    notificationStack: []
  },
  error: null,
  status: EntityStatus.notLoaded
};

export function downloadReportsEntityReducer(
  state: DownloadReportsEntityState = DownloadReportsEntityInitialState,
  action: DownloadReportsActions): DownloadReportsEntityState {
  switch (action.type) {

    case DownloadReportsActionTypes.ADD:
      const ackId = action.payload;
      return set('list.notificationStack', [...state.list.notificationStack, ackId], state);

    default:
      return state;
  }
}

