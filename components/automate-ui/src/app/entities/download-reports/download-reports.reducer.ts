import { HttpErrorResponse } from '@angular/common/http';
import { set } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { DownloadReportsActionTypes, DownloadReportsActions } from './download-reports.actions';

export interface DownloadReportsEntityState {
  list: {
    notificationItems: {}
  };
  error: HttpErrorResponse;
  status: EntityStatus;
}

export const DownloadReportsEntityInitialState: DownloadReportsEntityState = {
  list: {
    notificationItems: {}
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
      return set('list.notificationItems', {...state.list.notificationItems, ['ack_' + ackId]: 'running'}, state);
    case DownloadReportsActionTypes.CLEAR:
      const ack_Id = action.payload;
      const cloneState = JSON.parse(JSON.stringify(state.list.notificationItems));
      for (const props in cloneState) {
        if (props === ('ack_' + ack_Id)) {
          delete cloneState['ack_' + ack_Id];
        }
      }
      return set('list.notificationItems', { ...cloneState }, state);
    default:
      return state;
  }
}
