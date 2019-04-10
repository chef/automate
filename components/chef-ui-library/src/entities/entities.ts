import map from 'lodash/fp/map';
import pipe from 'lodash/fp/pipe';
import reduce from 'lodash/fp/reduce';
import set from 'lodash/fp/set';

export interface Entity {
  readonly id: string;
}

export interface IndexedEntities<T> {
  readonly [id: string]: T;
}

export const indexer: <T extends Entity>(transform: (object) => T) => (object) => IndexedEntities<T> =
  (transform) => pipe(map(transform),
                      reduce((acc, entity) => set(entity.id, entity, acc), {}));
