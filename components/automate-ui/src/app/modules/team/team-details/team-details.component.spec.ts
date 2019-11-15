import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { Router } from '@angular/router';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { routerReducer } from '@ngrx/router-store';
import { MockComponent } from 'ng2-mock-component';
import { StoreModule, Store, Action } from '@ngrx/store';

import * as routerStore from '@ngrx/router-store';
import { NgrxStateAtom, runtimeChecks } from 'app/ngrx.reducers';
import {
  projectsFilterReducer,
  projectsFilterInitialState
} from 'app/services/projects-filter/projects-filter.reducer';
import {
  policyEntityReducer, PolicyEntityInitialState
} from 'app/entities/policies/policy.reducer';
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
import {
  teamEntityReducer,
  TeamEntityInitialState
} from 'app/entities/teams/team.reducer';
import {
  GetTeamSuccess,
  GetTeamUsersSuccess,
  GetTeamUsers,
  GetTeam
} from 'app/entities/teams/team.actions';
import { Team } from 'app/entities/teams/team.model';
import { TeamDetailsComponent } from './team-details.component';

const declarations: any[] = [
  MockComponent({ selector: 'app-settings-sidebar' }),
  MockComponent({ selector: 'app-user-table',
    inputs: [
      'baseUrl',
      'users',
      'removeText',
      'addButtonText',
      'addButtonEnabled',
      'showEmptyMessage',
      'showTable',
      'overridePermissionsCheck']
  }),
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
];
const targetId = 'a-team-uuid-01';
const someTeam: Team = {
  id: targetId,
  name: 'some team',
  guid: targetId,
  projects: []
};

describe('TeamDetailsComponent v2', () => {
  let component: TeamDetailsComponent;
  let fixture: ComponentFixture<TeamDetailsComponent>;
  let router: Router;
  let store: Store<NgrxStateAtom>;

  const v2PolicyEntityInitialState = Object.assign({}, PolicyEntityInitialState,
    {iamMajorVersion: 'v2'});
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
    policies: v2PolicyEntityInitialState,
    projects: ProjectEntityInitialState,
    projectsFilter: projectsFilterInitialState
  };

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: declarations,
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
        }, { initialState, runtimeChecks })
      ]
    }).compileComponents();
  }));

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
      expect(component.users.length).toEqual(0);
    });
  });

  it('handles team users for v2', () => {
    spyOn(store, 'dispatch').and.callThrough();
    component.isIAMv2$.subscribe(isV2 => expect(isV2).toBeTruthy());
    const team: Team = { id: targetId, guid: 'any', name: 'any', projects: [] };
    store.dispatch(new GetTeamSuccess(team));

    expect(store.dispatch).toHaveBeenCalledWith(new GetTeamUsers({ id: targetId }));
    expect(store.dispatch).toHaveBeenCalledWith(new GetProjects());
  });

  it('initializes dropdown with those included on the team checked', () => {
    spyOn(store, 'dispatch').and.callThrough();
    component.isIAMv2$.subscribe(isV2 => expect(isV2).toBeTruthy());
    const teamProjects = ['b-proj', 'd-proj'];
    const team: Team = { id: targetId, guid: 'any', name: 'any', projects: teamProjects };
    store.dispatch(new GetTeamSuccess(team));
    expect(store.dispatch).toHaveBeenCalledWith(new GetProjects());

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

  function genProject(id: string): Project {
    return {
      id,
      status: 'NO_RULES', // unused
      name: id, // unused
      type: 'CUSTOM' // unused
    };
  }
});

describe('TeamDetailsComponent v1', () => {
  let component: TeamDetailsComponent;
  let fixture: ComponentFixture<TeamDetailsComponent>;
  let router: Router;
  let store: Store<NgrxStateAtom>;

  const v1PolicyEntityInitialState = Object.assign({}, PolicyEntityInitialState,
    {iamMajorVersion: 'v1'});
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
    policies: v1PolicyEntityInitialState,
    projects: ProjectEntityInitialState,
    projectsFilter: projectsFilterInitialState
  };

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: declarations,
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
        }, { initialState, runtimeChecks })
      ]
    }).compileComponents();
  }));

  beforeEach(() => {
    router = TestBed.get(Router);
    spyOn(router, 'navigate').and.stub();
    store = TestBed.get(Store);

    fixture = TestBed.createComponent(TeamDetailsComponent);
    component = fixture.componentInstance;
    component.team = someTeam;
    fixture.detectChanges();
  });

  it('handles team users for v1', () => {
    spyOn(store, 'dispatch').and.callThrough();
    const team: Team = { id: 'any', guid: targetId, name: 'any', projects: [] };
    component.isIAMv2$.subscribe(isV2 => expect(isV2).toBeFalsy());
    store.dispatch(new GetTeamSuccess(team));

    expect(store.dispatch).toHaveBeenCalledWith(new GetTeamUsers({ id: targetId }));
    expect(store.dispatch).not.toHaveBeenCalledWith(new GetProjects());
  });

  it('GetTeam is not requested when switching to the users page', () => {
    spyOn(store, 'dispatch').and.callThrough();
    component.isIAMv2$.subscribe(isV2 => expect(isV2).toBeFalsy());
    // This simulates updating the URL.
    store.dispatch(new GetRoute({
        'routerState': {
          'url': '/settings/users/admin', // <- update of the url to switch to the user's page for admin
          'params': {
            'id': 'admin'
          },
          'queryParams': {},
          'fragment': null,
          'path': ['/', 'settings', 'users', 'admin']
        }, 'event': {
          'id': 2,
          'url': '/settings/users/admin',
          'urlAfterRedirects': '/settings/users/admin',
          'state': {
            'url': '/settings/users/admin',
            'params': {
              'id': 'admin'
            },
            'queryParams': {},
            'fragment': null,
            'path': ['/', 'settings', 'users', 'admin']}
        }
      }));

    // GetTeam should not be called because we are not on the teams page any more.
    expect(store.dispatch).not.toHaveBeenCalledWith(new GetTeam({id: 'admin'}));
  });
});

export class GetRoute implements Action {
  readonly type = routerStore.ROUTER_NAVIGATION;

  constructor(public payload: any) { }
}
