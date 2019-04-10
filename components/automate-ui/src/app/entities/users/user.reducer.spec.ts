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
  GetUserByUsernameSuccess,
  GetUserByUsername,
  UpdateUser,
  UpdateUserSuccess,
  DeleteUserSuccess,
  GetUserByUsernameFailure,
  UpdateUserFailure,
  DeleteUser,
  DeleteUserFailure
} from './user.actions';
import { User } from './user.model';

describe('userStatusEntityReducer', () => {
  const initialState: UserEntityState = UserEntityInitialState;
  const httpErrorResponse = new HttpErrorResponse({ status: 400 });
  const user: User = {
    id: 'a953c5bb-82a5-41be-b7dd-a5de1ea53ada',
    username: 'test',
    name: 'test user'
  };
  const user2: User = {
    id: 'b953c5bb-82a5-41be-b7dd-a5de1ea53ada',
    username: 'test2',
    name: 'test user2'
  };
  const users: GetUsersSuccessPayload = {
    users: [ user, user2 ]
  };

  describe('User action types', () => {
    describe('GET_ALL', () => {
      const action = new GetUsers();

      it('sets status to loading', () => {
        const { status } = userEntityReducer(initialState, action);
        expect(status).toEqual(EntityStatus.loading);
      });
    });

    describe('GET_ALL_SUCCESS', () => {
      const payload = users;
      const action = new GetUsersSuccess(payload);

      it('sets status to loadingSuccess', () => {
        const { status } = userEntityReducer(initialState, action);
        expect(status).toEqual(EntityStatus.loadingSuccess);
      });
    });

    describe('GET_ALL_FAILURE', () => {
      const payload = httpErrorResponse;
      const action = new GetUsersFailure(payload);

      it('sets status to loadingFailure', () => {
        const { status } = userEntityReducer(initialState, action);
        expect(status).toEqual(EntityStatus.loadingFailure);
      });
    });

    describe('CREATE', () => {
      const payload: CreateUserPayload = {
        name: 'test user',
        username: 'test',
        password: 'testing123!'
      };
      const action = new CreateUser(payload);

      it('sets status to loading', () => {
        const { status } = userEntityReducer(initialState, action);
        expect(status).toEqual(EntityStatus.loading);
      });
    });

    describe('CREATE_SUCCESS', () => {
      const payload: User = user;
      const action = new CreateUserSuccess(payload);

      it('sets status to loadingSuccess', () => {
        const { status } = userEntityReducer(initialState, action);
        expect(status).toEqual(EntityStatus.loadingSuccess);
      });
    });

    describe('CREATE_FAILURE', () => {
      const payload = httpErrorResponse;
      const action = new CreateUserFailure(payload);

      it('sets status to loadingFailure', () => {
        const { status } = userEntityReducer(initialState, action);
        expect(status).toEqual(EntityStatus.loadingFailure);
      });
    });

    describe('GET_BY_USERNAME', () => {
      const payload = { username: 'test' };
      const action = new GetUserByUsername(payload);

      it('sets status to loading', () => {
        const { status } = userEntityReducer(initialState, action);
        expect(status).toEqual(EntityStatus.loading);
      });
    });

    describe('GET_BY_USERNAME_SUCCESS', () => {
      const payload = user;
      const action = new GetUserByUsernameSuccess(payload);

      it('sets status to loadingSuccess', () => {
        const prevState = { ...initialState, state: EntityStatus.loading };
        const { status } = userEntityReducer(prevState, action);
        expect(status).toEqual(EntityStatus.loadingSuccess);
      });

      it('loads the one user into the users state', () => {
        const { entities } = userEntityReducer(initialState, action);
        expect(Object.keys(entities)).toEqual(['a953c5bb-82a5-41be-b7dd-a5de1ea53ada']);
        expect(entities['a953c5bb-82a5-41be-b7dd-a5de1ea53ada']).toEqual(user);
      });
    });

    describe('GET_BY_USERNAME_FAILURE', () => {
      const payload = httpErrorResponse;
      const action = new GetUserByUsernameFailure(payload);

      it('sets status to loadingSuccess', () => {
        const prevState = { ...initialState, state: EntityStatus.loading };
        const { status } = userEntityReducer(prevState, action);
        expect(status).toEqual(EntityStatus.loadingFailure);
      });
    });

    describe('UPDATE', () => {
      const payload = user;
      const action = new UpdateUser(payload);

      it('sets status to loading', () => {
        const { updateStatus } = userEntityReducer(initialState, action);
        expect(updateStatus).toEqual(EntityStatus.loading);
      });
    });

    describe('UPDATE_SUCCESS', () => {
      const payload = { ...user, name: 'test user 123'};
      const action = new UpdateUserSuccess(payload);

      it('sets status to loadingSuccess', () => {
        const prevState = { ...initialState, state: EntityStatus.loading };
        const { updateStatus } = userEntityReducer(prevState, action);
        expect(updateStatus).toEqual(EntityStatus.loadingSuccess);
      });


      using([
        [initialState, userState(true), 'initial state'],
        [userState(true, user), userState(true, payload), 'one-user state'],
        [userState(true, user, user2), userState(true, payload, user2), 'two-user state']
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
        const { status } = userEntityReducer(initialState, action);
        expect(status).toEqual(EntityStatus.loading);
      });
    });

    describe('DELETE_SUCCESS', () => {
      const payload = { ...user, name: 'test user 123'};
      const action = new DeleteUserSuccess(payload);

      it('sets status to loadingSuccess', () => {
        const prevState = { ...initialState, state: EntityStatus.loading };
        const { status } = userEntityReducer(prevState, action);
        expect(status).toEqual(EntityStatus.loadingSuccess);
      });

      using([
        [initialState, userState(false), 'initial state'],
        [userState(false, user), userState(false), 'one user state'],
        [userState(false, user, user2), userState(false, user2), 'state with two users']
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

      it('sets status to loadingSuccess', () => {
        const prevState = { ...initialState, state: EntityStatus.loading };
        const { status } = userEntityReducer(prevState, action);
        expect(status).toEqual(EntityStatus.loadingFailure);
      });
    });
  });
});

function userState(isUpdate: boolean, ...us: User[]): UserEntityState {
  const entities: { [id: string]: User } = {};
  const ids: string[] = [];
  for (const u of us) {
    entities[u.id] = u;
    ids.push(u.id);
  }
  return {
    updateStatus: isUpdate ? EntityStatus.loadingSuccess : EntityStatus.notLoaded,
    status: isUpdate ? EntityStatus.notLoaded : EntityStatus.loadingSuccess,
    entities,
    ids
  };
}




