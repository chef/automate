import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { Router } from '@angular/router';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { routerReducer } from '@ngrx/router-store';
import { MockComponent } from 'ng2-mock-component';
import { StoreModule, Store } from '@ngrx/store';

import { using } from 'app/testing/spec-helpers';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import {
  ApiTokenEntityInitialState,
  apiTokenEntityReducer
} from 'app/entities/api-tokens/api-token.reducer';
import {
  GetTokenSuccess
} from 'app/entities/api-tokens/api-token.actions';
import { ApiToken } from 'app/entities/api-tokens/api-token.model';
import { ApiTokenDetailsComponent } from './api-token-details.component';
import {
  policyEntityReducer,
  PolicyEntityInitialState
} from 'app/entities/policies/policy.reducer';
import {
  projectEntityReducer,
  ProjectEntityInitialState
} from 'app/entities/projects/project.reducer';
import {
  projectsFilterReducer,
  projectsFilterInitialState
} from 'app/services/projects-filter/projects-filter.reducer';
import { Project } from 'app/entities/projects/project.model';
import { GetProjectsSuccess, GetProjects } from 'app/entities/projects/project.actions';
import { IamVersionResponse } from 'app/entities/policies/policy.requests';
import { GetIamVersionSuccess } from 'app/entities/policies/policy.actions';
import { IAMMinorVersion, IAMMajorVersion } from 'app/entities/policies/policy.model';

describe('ApiTokenDetailsComponent', () => {
  let component: ApiTokenDetailsComponent;
  let fixture: ComponentFixture<ApiTokenDetailsComponent>;
  let router: Router;
  let store: Store<NgrxStateAtom>;

  const targetId = 'a-token-01';
  const initialState = {
    router: {
      state: {
        url: `/settings/tokens/${targetId}`,
        params: { id: targetId },
        queryParams: {},
        fragment: ''
      },
      navigationId: 0
    },
    apiTokens: ApiTokenEntityInitialState,
    policies: PolicyEntityInitialState,
    projects: ProjectEntityInitialState,
    projectsFilter: projectsFilterInitialState
  };

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'app-admin-sidebar' }),
        MockComponent({ selector: 'input', inputs: ['resetOrigin'] }),
        MockComponent({ selector: 'chef-breadcrumb', inputs: ['link'] }),
        MockComponent({ selector: 'chef-breadcrumbs' }),
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-input' }),
        MockComponent({ selector: 'chef-page-header' }),
        MockComponent({ selector: 'chef-option' }),
        MockComponent({ selector: 'chef-radio', inputs: ['resetOrigin'] }),
        MockComponent({ selector: 'chef-heading' }),
        MockComponent({ selector: 'chef-subheading' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'app-settings-sidebar' }),
        MockComponent({ selector: 'app-projects-dropdown',
          inputs: ['projects', 'disabled'], outputs: ['onProjectChecked'] }),
        MockComponent({ selector: 'chef-tab-selector',
          inputs: ['value', 'routerLink', 'fragment']
        }),
        ApiTokenDetailsComponent
      ],
      imports: [
        ReactiveFormsModule,
        RouterTestingModule,
        ChefPipesModule,
        StoreModule.forRoot({
          router: routerReducer,
          apiTokens: apiTokenEntityReducer,
          policies: policyEntityReducer,
          projects: projectEntityReducer,
          projectsFilter: projectsFilterReducer
        }, { initialState })
      ]
    }).compileComponents();
  }));

  const someToken: ApiToken = {
    id: targetId,
    name: 'some token',
    value: 'random',
    active: true,
    created_at: '',
    updated_at: '',
    projects: []
  };

  const projectList = [
    genProject('a-proj'),
    genProject('b-proj'),
    genProject('c-proj'),
    genProject('d-proj')
  ];

  beforeEach(() => {
    router = TestBed.get(Router);
    spyOn(router, 'navigate').and.stub();
    store = TestBed.get(Store);

    fixture = TestBed.createComponent(ApiTokenDetailsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });

  using([
    ['v2', 'v0'],
    ['v1', 'v0']
  ], function (major: IAMMajorVersion, minor: IAMMinorVersion) {
    it('does not fetch projects for unsupported IAM versions', () => {
      spyOn(store, 'dispatch').and.callThrough();
      const version: IamVersionResponse = { version: { major, minor } };
      store.dispatch(new GetIamVersionSuccess(version));

      const token = { ...someToken, projects: ['b-proj', 'd-proj'] };
      store.dispatch(new GetTokenSuccess(token));
      expect(store.dispatch).not.toHaveBeenCalledWith(new GetProjects());
    });
  });

  it('fills in token on load for v2p1', () => {
    spyOn(store, 'dispatch').and.callThrough();
    const version: IamVersionResponse = { version: { major: 'v2', minor: 'v1' } };
    store.dispatch(new GetIamVersionSuccess(version));

    expect(component.token).toEqual(undefined);
    const token = { ...someToken, projects: ['b-proj', 'd-proj']};
    store.dispatch(new GetTokenSuccess(token));
    expect(store.dispatch).toHaveBeenCalledWith(new GetProjects());
    expect(component.token).toEqual(token);
  });

  it('initializes dropdown with those included on the token checked for v2p1', () => {
    spyOn(store, 'dispatch').and.callThrough();
    const version: IamVersionResponse = { version: { major: 'v2', minor: 'v1' } };
    store.dispatch(new GetIamVersionSuccess(version));
    const tokenProjects  = ['b-proj', 'd-proj'];
    store.dispatch(new GetTokenSuccess({...someToken, projects: tokenProjects}));

    expect(Object.keys(component.projects).length).toBe(0);
    store.dispatch(new GetProjectsSuccess({ projects: projectList }));
    expect(Object.keys(component.projects).length).toBe(projectList.length);

    projectList.forEach(p => {
      expect(component.projects[p.id].checked).toEqual(tokenProjects.includes(p.id));
    });
   });

  it('sets projects dirty when an unchecked project is checked', () => {
    store.dispatch(new GetTokenSuccess(
      { ...someToken, projects: ['b-proj', 'd-proj']}));
    store.dispatch(new GetProjectsSuccess({ projects: projectList }));

    expect(component.updateForm.controls.projects.pristine).toEqual(true);
    component.onProjectChecked({ ...genProject('a-proj'), checked: true });
    expect(component.updateForm.controls.projects.pristine).toEqual(false);
  });

  it('sets projects dirty when a checked project is unchecked', () => {
    store.dispatch(new GetTokenSuccess(
      { ...someToken, projects: ['b-proj', 'd-proj']}));
    store.dispatch(new GetProjectsSuccess({ projects: projectList }));

    expect(component.updateForm.controls.projects.pristine).toEqual(true);
    component.onProjectChecked({ ...genProject('b-proj'), checked: false });
    expect(component.updateForm.controls.projects.pristine).toEqual(false);
  });

  it('sets projects back to pristine when project changed back to original value', () => {
    store.dispatch(new GetTokenSuccess(
      { ...someToken, projects: ['b-proj', 'd-proj']}));
    store.dispatch(new GetProjectsSuccess({ projects: projectList }));

    expect(component.updateForm.controls.projects.pristine).toEqual(true);
    component.onProjectChecked({ ...genProject('a-proj'), checked: true });
    expect(component.updateForm.controls.projects.pristine).toEqual(false);
    component.onProjectChecked({ ...genProject('a-proj'), checked: false });
    expect(component.updateForm.controls.projects.pristine).toEqual(true);
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
