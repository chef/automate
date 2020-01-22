import { HttpErrorResponse } from '@angular/common/http';

import { using } from 'app/testing/spec-helpers';
import { EntityStatus } from '../entities';
import {
  userEntityReducer,
  UserEntityInitialState,
  UserEntityState
} from './user.reducer';
import {
  CreateUser,
  CreateUserFailure,
  CreateUserPayload,
  CreateUserSuccess,
  GetUsers,
  GetUsersFailure,
  GetUsersSuccess,
  GetUsersSuccessPayload,
  GetUserSuccess,
  GetUser,
  DeleteUserSuccess,
  UpdateNameUser,
  UpdateNameUserSuccess,
  GetUserFailure,
  UpdateUserFailure,
  DeleteUser,
  DeleteUserFailure
} from './user.actions';
import { User } from './user.model';

describe('userStatusEntityReducer', () => {
  const initialState: UserEntityState = UserEntityInitialState;
  const httpErrorResponse = new HttpErrorResponse({ status: 400 });
  const user: User = {
    id: 'test',
    name: 'test user',
    membership_id: 'a953c5bb-82a5-41be-b7dd-a5de1ea53ada'
  };
  const user2: User = {
    id: 'test2',
    name: 'test user2',
    membership_id: 'b953c5bb-82a5-41be-b7dd-a5de1ea53ada'
  };
  const users: GetUsersSuccessPayload = {
    users: [ user, user2 ]
  };

  describe('User action types', () => {
    describe('GET_ALL', () => {
      const action = new GetUsers();

      it('sets status to loading', () => {
        const { getStatus } = userEntityReducer(initialState, action);
        expect(getStatus).toEqual(EntityStatus.loading);
      });
    });

    describe('GET_ALL_SUCCESS', () => {
      const payload = users;
      const action = new GetUsersSuccess(payload);

      it('sets status to loadingSuccess', () => {
        const { getStatus } = userEntityReducer(initialState, action);
        expect(getStatus).toEqual(EntityStatus.loadingSuccess);
      });

      using([
        [initialState, userState('get', user, user2), 'initial state'],
        [userState('get', user), userState('get', user, user2), 'one-user state']
      ], (prevState, nextState: UserEntityState, descr: string) => {
        it(`with ${descr} fetches latest users`, () => {
          const actualNext = userEntityReducer(prevState, action);
          expect(actualNext).toEqual(nextState);
        });
      });
    });

    describe('GET_ALL_FAILURE', () => {
      const payload = httpErrorResponse;
      const action = new GetUsersFailure(payload);

      it('sets status to loadingFailure', () => {
        const { getStatus } = userEntityReducer(initialState, action);
        expect(getStatus).toEqual(EntityStatus.loadingFailure);
      });
    });

    describe('CREATE', () => {
      const payload: CreateUserPayload = {
        id: 'test',
        name: 'test user',
        password: 'testing123!'
      };
      const action = new CreateUser(payload);

      it('sets status to loading', () => {
        const { createStatus } = userEntityReducer(initialState, action);
        expect(createStatus).toEqual(EntityStatus.loading);
      });
    });

    describe('CREATE_SUCCESS', () => {
      const payload: User = user;
      const action = new CreateUserSuccess(payload);

      it('sets status to loadingSuccess', () => {
        const { createStatus } = userEntityReducer(initialState, action);
        expect(createStatus).toEqual(EntityStatus.loadingSuccess);
      });

      using([
        [initialState, userState('create', user), 'initial state'],
        [userState('create', user2), userState('create', user2, user), 'one-user state']
      ], (prevState, nextState: UserEntityState, descr: string) => {
        it(`with ${descr} adds a new user to the state`, () => {
          const actualNext = userEntityReducer(prevState, action);
          expect(actualNext).toEqual(nextState);
        });
      });
    });

    describe('CREATE_FAILURE', () => {
      const payload = httpErrorResponse;
      const action = new CreateUserFailure(payload);

      it('sets status to loadingFailure', () => {
        const { createStatus, createError } = userEntityReducer(initialState, action);

        expect(createStatus).toEqual(EntityStatus.loadingFailure);
        expect(createError).toEqual(action.payload);
      });
    });

    describe('GET', () => {
      const payload = { id: 'test' };
      const action = new GetUser(payload);

      it('sets status to loading', () => {
        const { getStatus } = userEntityReducer(initialState, action);
        expect(getStatus).toEqual(EntityStatus.loading);
      });
    });

    describe('GET_SUCCESS', () => {
      const payload = user;
      const action = new GetUserSuccess(payload);

      it('sets status to loadingSuccess', () => {
        const prevState = { ...initialState, state: EntityStatus.loading };
        const { getStatus } = userEntityReducer(prevState, action);
        expect(getStatus).toEqual(EntityStatus.loadingSuccess);
      });

      it('loads the one user into the users state', () => {
        const { entities } = userEntityReducer(initialState, action);
        expect(Object.keys(entities)).toEqual([user.id]);
        expect(entities[user.id]).toEqual(user);
      });
    });

    describe('GET_FAILURE', () => {
      const payload = httpErrorResponse;
      const action = new GetUserFailure(payload);

      it('sets status to loadingSuccess', () => {
        const prevState = { ...initialState, state: EntityStatus.loading };
        const { getStatus } = userEntityReducer(prevState, action);
        expect(getStatus).toEqual(EntityStatus.loadingFailure);
      });
    });

    describe('UPDATE_NAME_USER', () => {
      const payload = user;
      const action = new UpdateNameUser(payload);

      it('sets status to loading', () => {
        const { updateStatus } = userEntityReducer(initialState, action);
        expect(updateStatus).toEqual(EntityStatus.loading);
      });
    });

    describe('UPDATE_NAME_USER_SUCCESS', () => {
      const payload = { ...user, name: 'test user 123'};
      const action = new UpdateNameUserSuccess(payload);

      it('sets status to loadingSuccess', () => {
        const prevState = { ...initialState, state: EntityStatus.loading };
        const { updateStatus } = userEntityReducer(prevState, action);
        expect(updateStatus).toEqual(EntityStatus.loadingSuccess);
      });

      using([
        [initialState, userState('update'), 'initial state'],
        [userState('update', user), userState('update', payload), 'one-user state'],
        [userState('update', user, user2), userState('update', payload, user2), 'two-user state']
      ], (prevState, nextState: UserEntityState, descr: string) => {
        it(`with ${descr} updates the user if it's in the state`, () => {
          const actualNext = userEntityReducer(prevState, action);
          expect(actualNext).toEqual(nextState);
        });
      });
    });

    describe('UPDATE_FAILURE', () => {
      const payload = httpErrorResponse;
      const action = new UpdateUserFailure(payload);

      it('sets status to loadingSuccess', () => {
        const prevState = { ...initialState, state: EntityStatus.loading };
        const { updateStatus } = userEntityReducer(prevState, action);
        expect(updateStatus).toEqual(EntityStatus.loadingFailure);
      });
    });

    describe('DELETE', () => {
      const payload = user;
      const action = new DeleteUser(payload);

      it('sets status to loading', () => {
        const { deleteStatus } = userEntityReducer(initialState, action);
        expect(deleteStatus).toEqual(EntityStatus.loading);
      });
    });

    describe('DELETE_SUCCESS', () => {
      const payload = { ...user, name: 'test user 123'};
      const action = new DeleteUserSuccess(payload);

      it('sets deleteStatus to loadingSuccess', () => {
        const prevState = { ...initialState, state: EntityStatus.loading };
        const { deleteStatus } = userEntityReducer(prevState, action);
        expect(deleteStatus).toEqual(EntityStatus.loadingSuccess);
      });

      using([
        [initialState, userState('delete'), 'initial state'],
        [userState('delete', user), userState('delete'), 'one user state'],
        [userState('delete', user, user2), userState('delete', user2), 'state with two users']
      ], (prevState, nextState: UserEntityState, descr: string) => {
        it(`with ${descr} removes the one user if it's in the state`, () => {
          const actualNext = userEntityReducer(prevState, action);
          expect(actualNext).toEqual(nextState);
        });
      });
    });

    describe('DELETE_FAILURE', () => {
      const payload = httpErrorResponse;
      const action = new DeleteUserFailure(payload);

      it('sets deleteStatus to loadingSuccess', () => {
        const prevState = { ...initialState, state: EntityStatus.loading };
        const { deleteStatus } = userEntityReducer(prevState, action);
        expect(deleteStatus).toEqual(EntityStatus.loadingFailure);
      });
    });
  });
});

function userState(action: string, ...us: User[]): UserEntityState {
  const entities: { [id: string]: User } = {};
  const ids: string[] = [];
  for (const u of us) {
    entities[u.id] = u;
    ids.push(u.id);
  }
  return ['get', 'create', 'update', 'delete']
    .reduce((state, a) => {
      state[a + 'Status'] = a === action ? EntityStatus.loadingSuccess : EntityStatus.notLoaded;
      return state;
    }, { entities, ids, createError: null }) as UserEntityState;
}
