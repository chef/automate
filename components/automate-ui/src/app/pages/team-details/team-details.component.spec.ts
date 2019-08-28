import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { Router } from '@angular/router';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { routerReducer } from '@ngrx/router-store';
import { MockComponent } from 'ng2-mock-component';
import { StoreModule, Store } from '@ngrx/store';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { using } from 'app/testing/spec-helpers';
import {
  projectsFilterReducer,
  projectsFilterInitialState
} from 'app/services/projects-filter/projects-filter.reducer';
import {
  policyEntityReducer, PolicyEntityInitialState
} from 'app/entities/policies/policy.reducer';
import { GetIamVersionSuccess } from 'app/entities/policies/policy.actions';
import { IamVersionResponse } from 'app/entities/policies/policy.requests';
import { IAMMajorVersion } from 'app/entities/policies/policy.model';
import {
  projectEntityReducer,
  ProjectEntityInitialState
} from 'app/entities/projects/project.reducer';
import { Project } from 'app/entities/projects/project.model';
import { GetProjectsSuccess, GetProjects } from 'app/entities/projects/project.actions';
import {
  userEntityReducer,
  UserEntityInitialState
} from 'app/entities/users/user.reducer';
import { GetUsersSuccess, GetUsers } from 'app/entities/users/user.actions';
import {
  teamEntityReducer,
  TeamEntityInitialState
} from 'app/entities/teams/team.reducer';
import {
  GetTeamSuccess,
  GetTeamUsersSuccess,
  GetTeamUsers
} from 'app/entities/teams/team.actions';
import { Team } from 'app/entities/teams/team.model';
import { TeamDetailsComponent } from './team-details.component';

describe('TeamDetailsComponent', () => {
  let component: TeamDetailsComponent;
  let fixture: ComponentFixture<TeamDetailsComponent>;
  let router: Router;
  let store: Store<NgrxStateAtom>;

  const targetId = 'a-team-uuid-01';
  const initialState = {
    router: {
      state: {
        url: `/settings/teams/${targetId}`,
        params: { id: targetId },
        queryParams: {},
        fragment: ''
      },
      navigationId: 0 // what's that zero?
    },
    users: UserEntityInitialState,
    teams: TeamEntityInitialState,
    policies: PolicyEntityInitialState,
    projects: ProjectEntityInitialState,
    projectsFilter: projectsFilterInitialState
  };

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'app-admin-sidebar' }),
        MockComponent({ selector: 'app-user-table',
          inputs: [
            'baseUrl',
            'users$',
            'removeText',
            'addButtonText',
            'addButtonEnabled',
            'overridePermissionsCheck']
        }),
        MockComponent({ selector: 'app-user-team-membership-table',
          inputs: ['usersToFilter', 'users$', 'removeText', 'addButtonText'] }),
        MockComponent({ selector: 'input', inputs: ['resetOrigin'] }),
        MockComponent({ selector: 'chef-breadcrumb', inputs: ['link'] }),
        MockComponent({ selector: 'chef-breadcrumbs' }),
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-input' }),
        MockComponent({ selector: 'chef-page-header' }),
        MockComponent({ selector: 'chef-option' }),
        MockComponent({ selector: 'chef-heading' }),
        MockComponent({ selector: 'chef-subheading' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'app-projects-dropdown',
          inputs: ['projects', 'disabled'], outputs: ['onProjectChecked'] }),
        MockComponent({ selector: 'chef-tab-selector',
          inputs: ['value', 'routerLink', 'fragment']
        }),
        TeamDetailsComponent
      ],
      imports: [
        ReactiveFormsModule,
        RouterTestingModule,
        StoreModule.forRoot({
          router: routerReducer,
          teams: teamEntityReducer,
          users: userEntityReducer,
          policies: policyEntityReducer,
          projects: projectEntityReducer,
          projectsFilter: projectsFilterReducer
        }, { initialState })
      ]
    }).compileComponents();
  }));

  const someTeam: Team = {
    id: targetId,
    name: 'some team',
    guid: targetId,
    projects: []
  };

  beforeEach(() => {
    router = TestBed.get(Router);
    spyOn(router, 'navigate').and.stub();
    store = TestBed.get(Store);

    fixture = TestBed.createComponent(TeamDetailsComponent);
    component = fixture.componentInstance;
    component.team = someTeam;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });

  it('defaults to showing users section', () => {
    expect(component.tabValue).toBe('users');
  });

  it('show users section when users tab is selected', () => {
    component.onSelectedTab({ target: { value: 'users' } });
    expect(component.tabValue).toBe('users');
  });

  it('show details section when details tab is selected', () => {
    component.onSelectedTab({ target: { value: 'details' } });
    expect(component.tabValue).toBe('details');
  });

  describe('empty state', () => {
    beforeEach(() => {
      store.dispatch(new GetTeamSuccess(someTeam));
      store.dispatch(new GetTeamUsersSuccess({
        user_ids: []
      }));
      fixture.detectChanges();
    });

    it('users array should be empty', () => {
      component.sortedUsers$.subscribe((users) => {
        expect(users.length).toBe(0);
      });
    });
  });

  using([
    [targetId, 'other', 'V2'],
    ['other', targetId, 'V1']
  ], function (id: string, guid: string, major: IAMMajorVersion) {
    it(`initializes with fetching data for ${major} team users and projects`, () => {
      spyOn(store, 'dispatch').and.callThrough();
      const team: Team = { id, guid, name: 'any', projects: [] };
      store.dispatch(new GetTeamSuccess(team));
      const version: IamVersionResponse = { version: { major: major, minor: 'v0' } };
      store.dispatch(new GetIamVersionSuccess(version));

      expect(store.dispatch).toHaveBeenCalledWith(new GetUsers());
      expect(store.dispatch).toHaveBeenCalledWith(new GetTeamUsers({ id: targetId }));
      expect(store.dispatch).toHaveBeenCalledWith(new GetProjects());
    });
  });

  it('initializes dropdown with those included on the team checked', () => {
    const teamProjects = ['b-proj', 'd-proj'];
    const team: Team = { id: targetId, guid: 'any', name: 'any', projects: teamProjects };
    store.dispatch(new GetTeamSuccess(team));

    const version: IamVersionResponse = { version: { major: 'v2', minor: 'v1' } };
    store.dispatch(new GetIamVersionSuccess(version));

    const projectList = [
      genProject('a-proj'),
      genProject('b-proj'),
      genProject('c-proj'),
      genProject('d-proj')
    ];
    store.dispatch(new GetProjectsSuccess({ projects: projectList }));

    projectList.forEach(p => {
      expect(component.projects[p.id].checked).toEqual(teamProjects.includes(p.id));
    });
   });

  describe('sortedUsers$', () => {

    it('intermixes capitals and lowercase with lowercase first', () => {
      store.dispatch(new GetTeamUsersSuccess({ user_ids: ['user-id-1', 'user-id-2'] }));
      store.dispatch(new GetUsersSuccess({ users: [
        { membership_id: 'user-id-1', name: 'Alice', id: 'alice1' },
        { membership_id: 'user-id-2', name: 'alice', id: 'alice2' }
      ]}));
      component.sortedUsers$.subscribe(users => {
        expect(users.length).toBe(2);
        expect(users[0]).toEqual(jasmine.objectContaining({ name: 'alice' }));
        expect(users[1]).toEqual(jasmine.objectContaining({ name: 'Alice' }));
      });
    });

    it('sorts by whole string before case', () => {
      store.dispatch(new GetTeamUsersSuccess({
        user_ids: ['user-id-1', 'user-id-20', 'user-id-2']
      }));
      store.dispatch(new GetUsersSuccess({ users: [
        { membership_id: 'user-id-1', name: 'alice in wonderland', id: 'alice1' },
        { membership_id: 'user-id-20', name: 'alice', id: 'alice2' },
        { membership_id: 'user-id-2', name: 'Alice', id: 'alice3' }
      ]}));
      component.sortedUsers$.subscribe(users => {
        expect(users.length).toBe(3);
        expect(users[0]).toEqual(jasmine.objectContaining({ name: 'alice' }));
        expect(users[1]).toEqual(jasmine.objectContaining({ name: 'Alice' }));
        expect(users[2]).toEqual(jasmine.objectContaining({ name: 'alice in wonderland' }));
      });
    });

    it('sorts by name then by username', () => {
      store.dispatch(new GetTeamUsersSuccess({
        user_ids: ['user-id-22', 'user-id-1', 'user-id-20', 'user-id-2'] }));
      store.dispatch(new GetUsersSuccess({ users: [
       { membership_id: 'user-id-22', name: 'Bob', id: 'builder2001' },
       { membership_id: 'user-id-2', name: 'Bob', id: 'builder2000' },
       { membership_id: 'user-id-1', name: 'Alice in Wonderland', id: 'alice' },
       { membership_id: 'user-id-20', name: 'alice', id: 'the-other-alice' }
     ]}));
     component.sortedUsers$.subscribe(users => {
       expect(users.length).toBe(4);
       expect(users[0]).toEqual(jasmine.objectContaining({ name: 'alice' }));
       expect(users[1]).toEqual(jasmine.objectContaining({ name: 'Alice in Wonderland' }));
       expect(users[2]).toEqual(
         jasmine.objectContaining({ name: 'Bob', id: 'builder2000' }));
       expect(users[3]).toEqual(
         jasmine.objectContaining({ name: 'Bob', id: 'builder2001' }));
     });
    });

    it('uses natural ordering in name', () => {
      store.dispatch(new GetTeamUsersSuccess({
        user_ids: ['user-id-1', 'user-id-2', 'user-id-3', 'user-id-4', 'user-id-5'] }));
      store.dispatch(new GetUsersSuccess({ users: [
        { membership_id: 'user-id-1', name: 'Alice01', id: 'alice1' },
        { membership_id: 'user-id-2', name: 'Alice300', id: 'alice2' },
        { membership_id: 'user-id-3', name: 'Alice3', id: 'alice3' },
        { membership_id: 'user-id-4', name: 'Alice-2', id: 'alice4' },
        { membership_id: 'user-id-5', name: 'alice', id: 'alice5' }
      ]}));
      component.sortedUsers$.subscribe(users => {
        expect(users.length).toBe(5);
        expect(users[0]).toEqual(jasmine.objectContaining({ name: 'alice' }));
        expect(users[1]).toEqual(jasmine.objectContaining({ name: 'Alice-2' }));
        expect(users[2]).toEqual(jasmine.objectContaining({ name: 'Alice01' }));
        expect(users[3]).toEqual(jasmine.objectContaining({ name: 'Alice3' }));
        expect(users[4]).toEqual(jasmine.objectContaining({ name: 'Alice300' }));
      });
    });

    it('uses natural ordering in username', () => {
      store.dispatch(new GetTeamUsersSuccess({
        user_ids: ['user-id-1', 'user-id-2', 'user-id-3', 'user-id-4', 'user-id-5'] }));
      store.dispatch(new GetUsersSuccess({ users: [
        { membership_id: 'user-id-1', name: 'Alice', id: 'Alice01' },
        { membership_id: 'user-id-2', name: 'Alice', id: 'Alice300' },
        { membership_id: 'user-id-3', name: 'Alice', id: 'Alice3' },
        { membership_id: 'user-id-4', name: 'Alice', id: 'Alice-2' },
        { membership_id: 'user-id-5', name: 'Alice', id: 'alice' }
      ]}));
      component.sortedUsers$.subscribe(users => {
        expect(users.length).toBe(5);
        expect(users[0]).toEqual(jasmine.objectContaining({ id: 'alice' }));
        expect(users[1]).toEqual(jasmine.objectContaining({ id: 'Alice-2' }));
        expect(users[2]).toEqual(jasmine.objectContaining({ id: 'Alice01' }));
        expect(users[3]).toEqual(jasmine.objectContaining({ id: 'Alice3' }));
        expect(users[4]).toEqual(jasmine.objectContaining({ id: 'Alice300' }));
      });
    });
  });

  function genProject(id: string): Project {
    return {
      id,
      status: 'NO_RULES', // unused
      name: id, // unused
      type: 'CUSTOM' // unused
    };
  }
});
