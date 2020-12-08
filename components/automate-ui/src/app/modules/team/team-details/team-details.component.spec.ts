import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { Router } from '@angular/router';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { StoreModule, Store } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';

import {
  NgrxStateAtom,
  ngrxReducers,
  defaultInitialState,
  runtimeChecks,
  defaultRouterState,
  defaultRouterRouterState
} from 'app/ngrx.reducers';
import { using } from 'app/testing/spec-helpers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { PolicyEntityInitialState } from 'app/entities/policies/policy.reducer';
import { GetProjects } from 'app/entities/projects/project.actions';
import {
  GetTeamSuccess,
  GetTeamUsersSuccess,
  GetTeamUsers
} from 'app/entities/teams/team.actions';
import { Team } from 'app/entities/teams/team.model';
import { TeamDetailsComponent } from './team-details.component';

const declarations: any[] = [
  MockComponent({ selector: 'app-user-table',
    inputs: [
      'baseUrl',
      'users',
      'removeText',
      'addButtonText',
      'addButtonEnabled',
      'showEmptyMessage',
      'showTable',
      'getPermissionsPath',
      'createPermissionsPath'
    ]
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
  MockComponent({ selector: 'app-projects-dropdown', inputs: ['checkedProjectIDs'] }),
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

describe('TeamDetailsComponent', () => {
  let component: TeamDetailsComponent;
  let fixture: ComponentFixture<TeamDetailsComponent>;
  let router: Router;
  let store: Store<NgrxStateAtom>;

  const initialState = {
    ...defaultInitialState,
    router: {
      ...defaultRouterState,
      state: {
        ...defaultRouterRouterState,
        url: `/settings/teams/${targetId}`,
        params: { id: targetId }
      }
    },
    policies: {
      ...PolicyEntityInitialState
    }
  };

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations,
      providers: [
        FeatureFlagsService
      ],
      imports: [
        ReactiveFormsModule,
        RouterTestingModule,
        StoreModule.forRoot(ngrxReducers, { initialState, runtimeChecks })
      ]
    }).compileComponents();
  }));

  beforeEach(() => {
    router = TestBed.inject(Router);
    spyOn(router, 'navigate').and.stub();
    store = TestBed.inject(Store);

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
        membership_ids: []
      }));
      fixture.detectChanges();
    });

    it('users array should be empty', () => {
      expect(component.users.length).toEqual(0);
    });
  });

  it('handles team users', () => {
    spyOn(store, 'dispatch').and.callThrough();
    const team: Team = { id: targetId, guid: 'any', name: 'any', projects: [] };
    store.dispatch(new GetTeamSuccess(team));

    expect(store.dispatch).toHaveBeenCalledWith(new GetTeamUsers({ id: targetId }));
    expect(store.dispatch).toHaveBeenCalledWith(new GetProjects());
  });

  using([
    ['no projects', []],
    ['one project', ['proj-one']],
    ['multiple projects', ['p1', 'p2', 'p3', 'p4']]
  ], function (description: string, projects: string[]) {
    it(`initializes dropdown with those included on the team for ${description}`, () => {
      const team: Team = { id: targetId, guid: 'any', name: 'any', projects };
      store.dispatch(new GetTeamSuccess(team));

      expect(component.team.projects).toEqual(projects);
    });
  });

  using([
    ['no projects', []],
    ['one project', ['proj-one']],
    ['multiple projects', ['p1', 'p2', 'p3', 'p4']]
  ], function (description: string, projects: string[]) {
    it(`transfers result from closing dropdown into form for ${description}`, () => {
      const originalProjects = ['to-be-overwritten'];
      const team: Team = { id: targetId, guid: 'any', name: 'any', projects: originalProjects };
      store.dispatch(new GetTeamSuccess(team));
      expect(component.team.projects).toEqual(originalProjects);

      component.onProjectDropdownClosing(projects);

      expect(component.updateForm.controls.projects.value).toEqual(projects);
    });
  });
});

