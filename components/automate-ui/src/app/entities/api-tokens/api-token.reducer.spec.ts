import { HttpErrorResponse } from '@angular/common/http';
import { cloneDeep, filter, keys } from 'lodash/fp';
import * as faker from 'faker';

import { using } from 'app/testing/spec-helpers';
import { EntityStatus } from '../entities';
import {
  apiTokenEntityReducer,
  ApiTokenEntityInitialState,
  ApiTokenEntityState
} from './api-token.reducer';
import {
  GetAllTokens,
  GetAllTokensSuccess,
  GetAllTokensFailure,
  GetAllTokensSuccessPayload,
  CreateToken,
  CreateTokenSuccess,
  CreateTokenFailure,
  ToggleTokenActiveSuccess,
  DeleteToken,
  DeleteTokenSuccess,
  DeleteTokenFailure
} from './api-token.actions';
import { ApiToken } from './api-token.model';

describe('apiTokenStatusEntityReducer', () => {
  const initialState: ApiTokenEntityState = ApiTokenEntityInitialState;

  // note: the error code doesn't matter, but it has to be passed to the constructor
  const httpErrorResponse = new HttpErrorResponse({ status: 400 });

  describe('Get Tokens', () => {

    describe('GET', () => {
      const action = new GetAllTokens();

      it('sets status to loading', () => {
        const { status } = apiTokenEntityReducer(initialState, action);
        expect(status).toEqual(EntityStatus.loading);
      });
    });

    describe('GET_SUCCESS', () => {
      const payload: GetAllTokensSuccessPayload = {
        tokens: [
          genToken(),
          genToken()
        ]
      };
      const existingTokenId = payload.tokens[0].id;
      const action = new GetAllTokensSuccess(payload);

      it('sets status to loadingSuccess', () => {
        const { status } = apiTokenEntityReducer(initialState, action);
        expect(status).toEqual(EntityStatus.loadingSuccess);
      });

      using([
        [initialState, 'initial state'],
        [genArbitraryState(), 'non-initial state']
      ], function (state: ApiTokenEntityState, name: string) {

        it('puts all tokens from the payload in the state for' + name, () => {
          const { entities } = apiTokenEntityReducer(state, action);

          const ids = keys(entities);

          // check number of entries
          expect(ids.length).toEqual(payload.tokens.length);

          // check all payload entries now in state
          payload.tokens.forEach(token => expect(ids).toContain(token.id));
        });
      });

      using([
        [initialState, 'initial state'],
        [genArbitraryState(existingTokenId), 'non-initial state']
      ], function (state: ApiTokenEntityState, name: string) {
        it('completely overwrites any existing tokens in the state for ' + name, () => {

          const { entities } = apiTokenEntityReducer(state, action);

          // check number of entries
          expect(keys(entities).length).toEqual(payload.tokens.length);

          // check value has been updated to payload's value for same id
          expect(entities[existingTokenId].value).toBe(payload.tokens[0].value);

          // check all other ids are no longer present in state
          keys(state.entities).forEach((key) => {
            expect(key in entities).toBe(key === existingTokenId);
          });
        });
      });
    });

    describe('GET_FAILURE', () => {
      const payload = httpErrorResponse;
      const action = new GetAllTokensFailure(payload);

      it('sets status to loadingFailure', () => {
        const { status } = apiTokenEntityReducer(initialState, action);
        expect(status).toEqual(EntityStatus.loadingFailure);
      });
    });
  });

  describe('Create Token', () => {

    describe('CREATE', () => {
      const action = new CreateToken({
          name: faker.lorem.words(),
          id: faker.lorem.words()
        });

      it('sets status to loading', () => {
        const { saveStatus } = apiTokenEntityReducer(initialState, action);
        expect(saveStatus).toEqual(EntityStatus.loading);
      });
    });

    describe('CREATE_SUCCESS', () => {
      const payload = genToken();
      const action = new CreateTokenSuccess(payload);

      it('sets status to loadingSuccess', () => {
        const { saveStatus } = apiTokenEntityReducer(initialState, action);
        expect(saveStatus).toEqual(EntityStatus.loadingSuccess);
      });

      using([
        [initialState, 'initial state'],
        [genArbitraryState(), 'non-initial state']
      ], function (state: ApiTokenEntityState, name: string) {
        it('adds token from the payload to the state for ' + name, () => {
          const { entities } = apiTokenEntityReducer(state, action);

          const ids = keys(entities);
          expect(ids.length).toEqual(keys(state.entities).length + 1);
          expect(ids).toContain(payload.id);
          expect(entities[payload.id]).toBe(payload);
        });
      });
    });

    describe('CREATE_FAILURE', () => {
      const payload = httpErrorResponse;
      const action = new CreateTokenFailure(payload);

      it('sets status to loadingFailure', () => {
        const { saveStatus } = apiTokenEntityReducer(initialState, action);
        expect(saveStatus).toEqual(EntityStatus.loadingFailure);
      });
    });
  });

  describe('Toggle Token Active', () => {

    it('toggles active status of token', () => {
      const targetId = faker.random.uuid();
      const state = genArbitraryState(targetId);
      const token = { ...state.entities[targetId], active: !state.entities[targetId].active };
      // ensure we have a copy!
      expect(state.entities[targetId].active).toBe(!token.active);

      const action = new ToggleTokenActiveSuccess(token);
      const { entities } = apiTokenEntityReducer(state, action);

      expect(entities[targetId].active).toBe(token.active);
      expect(entities[targetId].active).not.toBe(state.entities[targetId].active);
    });

    it('does not otherwise affect tokens present', () => {
      const targetId = faker.random.uuid();
      const state = genArbitraryState(targetId);
      const token = Object.assign({}, state.entities[targetId]);
      token.active = !token.active;

      const action = new ToggleTokenActiveSuccess(token);
      const { entities } = apiTokenEntityReducer(state, action);

      const ids = keys(entities);
      expect(ids.length).toEqual(keys(state.entities).length);
      filter(id => id !== targetId, keys(state.entities))
        .forEach(id => {
          expect(ids).toContain(id);
          expect(state.entities[id].active).toBe(entities[id].active);
        });
    });
  });

  describe('Delete Token', () => {
    describe('DELETE', () => {
      const token = genToken();
      const action = new DeleteToken(token);

      it('sets status to loading', () => {
        const { deleteStatus } = apiTokenEntityReducer(genArbitraryState(), action);
        expect(deleteStatus).toEqual(EntityStatus.loading);
      });
    });

    describe('DELETE_SUCCESS', () => {
      const token = genToken();
      const action = new DeleteTokenSuccess(token);

      it('resets token to delete', () => {
        const { tokenToDelete } = apiTokenEntityReducer(genArbitraryState(), action);
        expect(tokenToDelete).toBeNull();
      });

      it('sets status to loadingSuccess', () => {
        const { deleteStatus } = apiTokenEntityReducer(genArbitraryState(), action);
        expect(deleteStatus).toEqual(EntityStatus.loadingSuccess);
      });

      it('deletes token from state', () => {
        const state = genArbitraryState(token.id);

        const { entities } = apiTokenEntityReducer(state, action);

        expect(keys(entities).length).toEqual(keys(state.entities).length - 1);
        expect(keys(entities)).not.toContain(token.id);
      });

      it('leaves all other tokens present', () => {
        const state = genArbitraryState(token.id);

        const { entities } = apiTokenEntityReducer(state, action);

        keys(entities).forEach(id => expect(keys(state.entities)).toContain(id));
      });
    });

    describe('DELETE_FAILURE', () => {
      const payload = httpErrorResponse;
      const action = new DeleteTokenFailure(payload);

      it('resets token to delete', () => {
        const { tokenToDelete } = apiTokenEntityReducer(initialState, action);
        expect(tokenToDelete).toBeNull();
      });

      it('sets status to loadingFailure', () => {
        const { deleteStatus } = apiTokenEntityReducer(initialState, action);
        expect(deleteStatus).toEqual(EntityStatus.loadingFailure);
      });
    });
  });

  function genToken(tokenId?: string): ApiToken {
    return {
      id: tokenId ? tokenId : faker.random.uuid(),
      value: faker.lorem.word(),
      name: faker.lorem.words(),
      active: faker.random.boolean(),
      created_at: faker.date.past().toISOString(),
      updated_at: faker.date.recent().toISOString(),
      projects: [] // TODO add values when we expose projects in UI
    };
  }

  function genArbitraryState(tokenId?: string): ApiTokenEntityState {
    const newState: ApiTokenEntityState = cloneDeep(initialState);
    const existingTokenId = tokenId ? tokenId : faker.random.uuid();
    newState.entities[existingTokenId] = genToken(existingTokenId);
    const newToken2 = genToken();
    newState.entities[newToken2.id] = genToken(newToken2.id);
    const newToken3 = genToken();
    newState.entities[newToken3.id] = genToken(newToken3.id);
    return newState;
  }

});
