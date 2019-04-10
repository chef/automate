import { HttpErrorResponse } from '@angular/common/http';
import { cloneDeep, difference, keys, times } from 'lodash/fp';
import * as faker from 'faker';

import { using } from 'app/testing/spec-helpers';
import { EntityStatus } from '../entities';
import {
  teamEntityReducer,
  TeamEntityInitialState,
  TeamEntityState
} from './team.reducer';
import {
  GetTeam,
  GetTeamFailure,
  GetTeamSuccess,
  GetTeams,
  GetTeamsFailure,
  GetTeamsSuccess,
  GetTeamsSuccessPayload,
  CreateTeam,
  CreateTeamFailure,
  CreateTeamSuccess,
  CreateTeamPayload,
  UpdateTeam,
  UpdateTeamFailure,
  UpdateTeamSuccess,
  DeleteTeam,
  DeleteTeamFailure,
  DeleteTeamSuccess,
  AddTeamUsers,
  AddTeamUsersFailure,
  AddTeamUsersSuccess,
  TeamUserMgmtPayload,
  RemoveTeamUsers,
  RemoveTeamUsersFailure,
  RemoveTeamUsersSuccess
} from './team.actions';
import { Team } from './team.model';

describe('teamStatusEntityReducer', () => {
  const initialState: TeamEntityState = TeamEntityInitialState;
  const httpErrorResponse = new HttpErrorResponse({ status: 400 });

  describe('Team action types', () => {
    describe('GET_ALL', () => {
      const action = new GetTeams();

      it('sets status to loading', () => {
        const { getAllStatus } = teamEntityReducer(initialState, action);
        expect(getAllStatus).toEqual(EntityStatus.loading);
      });
    });

    describe('GET_ALL_SUCCESS', () => {
      const payload: GetTeamsSuccessPayload = {
        teams: [
          genTeam(),
          genTeam()
        ]
      };
      const existingTeamId = payload.teams[0].id;
      const action = new GetTeamsSuccess(payload);

      it('sets status to loadingSuccess', () => {
        const { getAllStatus } = teamEntityReducer(initialState, action);
        expect(getAllStatus).toEqual(EntityStatus.loadingSuccess);
      });

      using([
        [initialState, 'initial state'],
        [genArbitraryState(), 'non-initial state']
      ], function (state: TeamEntityState, description: string) {

        it('puts all teams from the payload in the state for' + description, () => {
          const { entities } = teamEntityReducer(state, action);

          const ids = keys(entities);

          expect(ids.length).toEqual(payload.teams.length);

          payload.teams.forEach(team => expect(ids).toContain(team.id));
        });
      });

      using([
        [initialState, 'initial state'],
        [genArbitraryState(existingTeamId), 'non-initial state']
      ], function (state: TeamEntityState, description: string) {
        it('completely overwrites any existing teams in the state for ' + description, () => {

          const { entities } = teamEntityReducer(state, action);

          expect(keys(entities).length).toEqual(payload.teams.length);

          expect(entities[existingTeamId].name).toBe(payload.teams[0].name);

          keys(state.entities).forEach((key) => {
            expect(key in entities).toBe(key === existingTeamId);
          });
        });
      });
    });

    describe('GET_ALL_FAILURE', () => {
      const payload = httpErrorResponse;
      const action = new GetTeamsFailure(payload);

      it('sets status to loadingFailure', () => {
        const { getAllStatus } = teamEntityReducer(initialState, action);
        expect(getAllStatus).toEqual(EntityStatus.loadingFailure);
      });
    });
  });

  describe('GET', () => {
    const team = genTeam();
    const teamId = team.id;
    const action = new GetTeam(({id: teamId}));

    it('sets status to loading', () => {
      const { getStatus } = teamEntityReducer(initialState, action);
      expect(getStatus).toEqual(EntityStatus.loading);
    });
  });

  describe('GET_SUCCESS', () => {
    const payload = genTeam();

    const existingTeamId = payload.id;
    const action = new GetTeamSuccess(payload);

    it('sets status to loadingSuccess', () => {
      const { getStatus } = teamEntityReducer(initialState, action);
      expect(getStatus).toEqual(EntityStatus.loadingSuccess);
    });

    using([
      [initialState, 'initial state'],
      [genArbitraryState(), 'non-initial state']
    ], function (state: TeamEntityState, description: string) {

      it('loads the user into the state for' + description, () => {
        const { entities } = teamEntityReducer(state, action);

        expect(Object.keys(entities)).toContain(existingTeamId);
        expect(entities[existingTeamId]).toEqual(payload);
      });
    });
  });

  describe('GET_FAILURE', () => {
    const action = new GetTeamFailure(httpErrorResponse);

    it('sets status to loadingFailure', () => {
      const { getStatus } = teamEntityReducer(initialState, action);
      expect(getStatus).toEqual(EntityStatus.loadingFailure);
    });
  });

  describe('CREATE', () => {
    const createPayload: CreateTeamPayload = {
      name: faker.random.word('name'),
      id: faker.random.word('id'),
      projects: [faker.random.word('project')]
    };

    const action = new CreateTeam(createPayload);

    it('sets status to loading', () => {
      const { createStatus } = teamEntityReducer(initialState, action);
      expect(createStatus).toEqual(EntityStatus.loading);
    });
  });

  describe('CREATE_SUCCESS', () => {
    const payload = genTeam();
    const action = new CreateTeamSuccess(payload);

    it('sets status to loadingSuccess', () => {
      const { createStatus } = teamEntityReducer(initialState, action);
      expect(createStatus).toEqual(EntityStatus.loadingSuccess);
    });

    using([
      [initialState, 'initial state'],
      [genArbitraryState(), 'non-initial state']
    ], function (state: TeamEntityState, description: string) {
      it('adds team from the payload to the state for ' + description, () => {
        const { entities } = teamEntityReducer(state, action);

        const ids = keys(entities);
        expect(ids.length).toEqual(keys(state.entities).length + 1);
        expect(ids).toContain(payload.id);
        expect(entities[payload.id]).toBe(payload);
      });
    });
  });

  describe('CREATE_FAILURE', () => {
    const payload = httpErrorResponse;
    const action = new CreateTeamFailure(payload);

    it('sets status to loadingFailure', () => {
      const { createStatus } = teamEntityReducer(initialState, action);
      expect(createStatus).toEqual(EntityStatus.loadingFailure);
    });
  });

  describe('UPDATE', () => {
    const payload = genTeam();
    const action = new UpdateTeam(payload);

    it('sets status to loading', () => {
      const { updateStatus } = teamEntityReducer(initialState, action);
      expect(updateStatus).toEqual(EntityStatus.loading);
    });
  });

  describe('UPDATE_SUCCESS', () => {
    const newName = 'test team 123';
    const newDesc = 'this team is for testing :)';
    const payload = { ...genTeam(), name: newName, description: newDesc };
    const teamId = payload.id;
    const action = new UpdateTeamSuccess(payload);

    it('sets status to loadingSuccess', () => {
      const { updateStatus } = teamEntityReducer(initialState, action);
      expect(updateStatus).toEqual(EntityStatus.loadingSuccess);
    });


    using([
      [genArbitraryState(teamId), 'non-initial state']
    ], (state: TeamEntityState, description: string) => {
      it('updates existing team in the state for ' + description, () => {
        const { entities } = teamEntityReducer(state, action);
        expect(entities[teamId].name).toEqual(newName);
        expect(entities[teamId].id).toEqual(teamId);
      });
    });
  });

  describe('UPDATE_FAILURE', () => {
    const payload = httpErrorResponse;
    const action = new UpdateTeamFailure(payload);

    it('sets status to loadingFailure', () => {
      const { updateStatus } = teamEntityReducer(initialState, action);
      expect(updateStatus).toEqual(EntityStatus.loadingFailure);
    });
  });

  describe('DELETE', () => {
    const action = new DeleteTeam(genTeam());

    it('sets status to loading', () => {
      const { deleteStatus } = teamEntityReducer(initialState, action);
      expect(deleteStatus).toEqual(EntityStatus.loading);
    });
  });

  describe('DELETE_SUCCESS', () => {
    const payload = genTeam();
    const teamId = payload.id;
    const action = new DeleteTeamSuccess(payload);

    it('sets status to loadingSuccess', () => {
      const { deleteStatus } = teamEntityReducer(initialState, action);
      expect(deleteStatus).toEqual(EntityStatus.loadingSuccess);
    });

    using([
      [genArbitraryState(teamId), 'non-initial state']
    ], (state: TeamEntityState, description: string) => {
      it('deletes existing team in the state for ' + description, () => {
        const { entities } = teamEntityReducer(state, action);
        expect(entities[teamId]).toBeUndefined();
      });
    });
  });

  describe('DELETE_FAILURE', () => {
    const payload = httpErrorResponse;
    const action = new DeleteTeamFailure(payload);

    it('sets status to loadingFailure', () => {
      const { deleteStatus } = teamEntityReducer(initialState, action);
      expect(deleteStatus).toEqual(EntityStatus.loadingFailure);
    });
  });

  describe('ADD_USERS', () => {
    const team = genTeam();
    const teamId = team.id;
    const payload = <TeamUserMgmtPayload>{
      id: teamId,
      user_ids: [
        faker.random.uuid(),
        faker.random.uuid()
      ]
    };
    const action = new AddTeamUsers(payload);

    it('sets status to loading', () => {
      const { addUsersStatus } = teamEntityReducer(initialState, action);
      expect(addUsersStatus).toEqual(EntityStatus.loading);
    });
  });

  describe('ADD_USERS_SUCCESS', () => {
    it('sets status to loadingSuccess', () => {
      const payload = genTeamUserMgmtPayload();
      const action = new AddTeamUsersSuccess(payload);

      const { addUsersStatus } = teamEntityReducer(initialState, action);
      expect(addUsersStatus).toEqual(EntityStatus.loadingSuccess);
    });

    using([
      [initialState, 'initial state', 0],
      [genArbitraryState(), 'non-initial state', 0],
      [genStateWithUserIDs(2), 'when there are already user IDs', 2]
    ], function (state: TeamEntityState, description: string, _initialUserIdLength: number) {
      it('adds users from the payload to the state for ' + description, () => {
        const userIDArray = [
          faker.random.uuid(),
          faker.random.uuid()
        ];
        const payload = <TeamUserMgmtPayload>{
          id: faker.random.uuid(),
          user_ids: userIDArray
        };
        const action = new AddTeamUsersSuccess(payload);
        const initUserIDs = cloneDeep(state.userIDs);

        const { userIDs } = teamEntityReducer(state, action);
        expect(userIDs).toEqual(initUserIDs.concat(userIDArray));
      });
    });
  });

  describe('ADD_USERS_FAILURE', () => {
    const action = new AddTeamUsersFailure(httpErrorResponse);

    it('sets status to loadingFailure', () => {
      const { addUsersStatus } = teamEntityReducer(initialState, action);
      expect(addUsersStatus).toEqual(EntityStatus.loadingFailure);
    });
  });

  describe('REMOVE_USERS', () => {
    const team = genTeam();
    const teamId = team.id;
    const payload = <TeamUserMgmtPayload>{
      id: teamId,
      user_ids: [
        faker.random.uuid()
      ]
    };
    const action = new RemoveTeamUsers(payload);

    it('sets status to loading', () => {
      const { removeUsersStatus } = teamEntityReducer(initialState, action);
      expect(removeUsersStatus).toEqual(EntityStatus.loading);
    });
  });

  describe('REMOVE_USERS_SUCCESS', () => {
    it('sets status to loadingSuccess', () => {
      const payload = genTeamUserMgmtPayload();
      const action = new RemoveTeamUsersSuccess(payload);
      const { removeUsersStatus } = teamEntityReducer(initialState, action);
      expect(removeUsersStatus).toEqual(EntityStatus.loadingSuccess);
    });

    const multipleState = genStateWithUserIDs(4);
    const usersToRemoveMultiple = multipleState.userIDs.slice(0, 1);
    using([
      [initialState, 'initial state with no inputs', 0, []],
      [genArbitraryState(), 'non-initial state', 0, [faker.random.uuid()]],
      [multipleState, 'when there are already user IDs', 4, usersToRemoveMultiple]
    ], function (state: TeamEntityState, description: string,
        _initialUserIdLength: number, usersToRemove: string[]) {
      it('removes users from the payload to the state for ' + description, () => {
        const payload = <TeamUserMgmtPayload>{
          id: faker.random.uuid(),
          user_ids: usersToRemove
        };
        const action = new RemoveTeamUsersSuccess(payload);
        const initUserIDs = cloneDeep(state.userIDs);

        const { userIDs } = teamEntityReducer(state, action);
        expect(userIDs).toEqual(difference(initUserIDs, usersToRemove));
      });
    });
  });

  describe('REMOVE_USERS_FAILURE', () => {
    const action = new RemoveTeamUsersFailure(httpErrorResponse);

    it('sets status to loadingFailure', () => {
      const { removeUsersStatus } = teamEntityReducer(initialState, action);
      expect(removeUsersStatus).toEqual(EntityStatus.loadingFailure);
    });
  });

  function genTeam(teamId?: string): Team {
    return {
      id: teamId || faker.random.uuid(),
      name: faker.random.word('string'),
      guid: faker.random.uuid(),
      projects: [faker.random.word('string')]
    };
  }

  function genTeamUserMgmtPayload(teamId?: string): TeamUserMgmtPayload {
    return {
      id: teamId || faker.random.uuid(),
      user_ids: [faker.random.uuid(), faker.random.uuid(), faker.random.uuid()]
    };
  }

  function genArbitraryState(teamId?: string): TeamEntityState {
    const newState: TeamEntityState = cloneDeep(initialState);
    const existingTeamId = teamId || faker.random.uuid();
    newState.entities[existingTeamId] = genTeam(existingTeamId);
    const newTeam2 = genTeam();
    newState.entities[newTeam2.id] = genTeam(newTeam2.id);
    const newTeam3 = genTeam();
    newState.entities[newTeam3.id] = genTeam(newTeam3.id);
    return newState;
  }

  function genStateWithUserIDs(numberOfMembers: number): TeamEntityState {
    const newState: TeamEntityState = cloneDeep(initialState);
    const existingTeamId = faker.random.uuid();
    newState.entities[existingTeamId] = genTeam(existingTeamId);
    newState.userIDs = times(() => faker.random.uuid(), numberOfMembers);
    return newState;
  }
});
