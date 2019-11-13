import {
  pipe,
  reduce,
  map,
  set
} from 'lodash/fp';

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

export function pending(status: EntityStatus): boolean {
  return status === EntityStatus.notLoaded || status === EntityStatus.loading;
}

export function loading(status: EntityStatus): boolean {
  return status === EntityStatus.loading;
}

export function allLoadedSuccessfully(statuses: EntityStatus[]): boolean {
  return statuses.every(status => status === EntityStatus.loadingSuccess);
}

export function allLoaded(statuses: EntityStatus[]): boolean {
  return statuses.every(status =>
    status === EntityStatus.loadingSuccess || status === EntityStatus.loadingFailure);
}
