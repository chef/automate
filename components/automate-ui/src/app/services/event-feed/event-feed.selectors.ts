import { createSelector } from '@ngrx/store';

export const eventFeedState = state => state.event_feed;
export const loadedEvents = createSelector(eventFeedState, state => state.loadedEvents);
export const completeNumberOfEvents = createSelector(eventFeedState,
    state => state.completeNumberOfEvents);
export const initialFeedStatus = createSelector(eventFeedState, state => state.initialFeedStatus);
export const guitarStringCollection =
    createSelector(eventFeedState, state => state.guitarStringCollection);
export const eventTypeCounts = createSelector(eventFeedState, state => state.typeCounts);
export const eventTaskCounts = createSelector(eventFeedState, state => state.taskCounts);
