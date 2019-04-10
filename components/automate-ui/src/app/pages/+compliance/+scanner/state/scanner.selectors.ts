import { createSelector } from '@ngrx/store';
import { omit } from 'lodash';

export const scannerState = state => state.scanner;

export const jobsList = createSelector(scannerState, state => state.jobsList);

export const jobsListItems = createSelector(jobsList, list => list.items);

export const jobsListParams = createSelector(jobsList, list => omit(list, 'items', 'total'));

export const jobDeletePrompt = createSelector(scannerState, state => state.jobDeletePrompt);

export const jobDetail = createSelector(scannerState, state => state.jobDetail);

export const jobScansList = createSelector(scannerState, state => state.jobScansList);

export const jobScansListParams = createSelector(jobScansList,
  list => omit(list, 'items', 'total'));

export const nodesList = createSelector(scannerState, state => state.nodesList);

export const nodesListItems = createSelector(nodesList, list => list.items);

export const nodesListParams = createSelector(nodesList, list => omit(list, 'items', 'total'));

export const nodeTotals = createSelector(scannerState, state => state.nodeTotals);

export const nodeDetail = createSelector(scannerState, state => state.nodeDetail);
