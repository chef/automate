import {
  pipe,
  reduce,
  map,
  set
} from 'lodash/fp';
import { Action } from '@ngrx/store';

export enum EntityStatus {
  notLoaded = 'notLoaded',
  loading = 'loading',
  loadingSuccess = 'loadingSuccess',
  loadingFailure = 'loadingFailure'
}

export interface Entity {
  readonly id: string;
}

export interface IndexedEntities<T> {
  readonly [id: string]: T;
}

export const indexer =
  <T extends Entity>(transform: (object) => T): (object) => IndexedEntities<T> => {
    return pipe(map(transform),
                reduce((acc, entity) => set(entity.id, entity, acc), {}));
  };

export interface StateWithStatus {
  status: EntityStatus;
}

export function pendingState(state: StateWithStatus): boolean {
  return (
    !state ||  // this line required ONLY for unit tests; won't happen in real life
    state.status === EntityStatus.notLoaded ||
    state.status === EntityStatus.loading
  );
}

export function loading(status: EntityStatus): boolean {
  return status === EntityStatus.loading;
}

export enum AppActionTypes {
  NOOP = 'APP::NOOP'
}
export class NoopAction implements Action {
  readonly type = AppActionTypes.NOOP;
}
