import { async, ComponentFixture, TestBed } from '@angular/core/testing';
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
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { Project } from 'app/entities/projects/project.model';
import { GetProjectsSuccess, GetProjects } from 'app/entities/projects/project.actions';
import { GetTokenSuccess } from 'app/entities/api-tokens/api-token.actions';
import { ApiToken } from 'app/entities/api-tokens/api-token.model';
import { ApiTokenDetailsComponent } from './api-token-details.component';

describe('ApiTokenDetailsComponent', () => {
  let component: ApiTokenDetailsComponent;
  let fixture: ComponentFixture<ApiTokenDetailsComponent>;
  let router: Router;
  let store: Store<NgrxStateAtom>;

  const targetId = 'a-token-01';
  const initialState = {
    ...defaultInitialState,
    router: {
      ...defaultRouterState,
      state: {
        ...defaultRouterRouterState,
        url: `/settings/tokens/${targetId}`,
        params: { id: targetId }
      }
    }
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
        MockComponent({ selector: 'app-projects-dropdown',
          inputs: ['projects', 'disabled'], outputs: ['onProjectChecked'] }),
        MockComponent({ selector: 'chef-tab-selector',
          inputs: ['value', 'routerLink', 'fragment']
        }),
        ApiTokenDetailsComponent
      ],
      providers: [
        FeatureFlagsService
      ],
      imports: [
        ReactiveFormsModule,
        RouterTestingModule,
        ChefPipesModule,
        StoreModule.forRoot(ngrxReducers, { initialState, runtimeChecks })
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
    router = TestBed.inject(Router);
    spyOn(router, 'navigate').and.stub();
    store = TestBed.inject(Store);

    fixture = TestBed.createComponent(ApiTokenDetailsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });

  it('fills in token on load', () => {
    spyOn(store, 'dispatch').and.callThrough();

    expect(component.token).toEqual(undefined);
    const token = { ...someToken, projects: ['b-proj', 'd-proj']};
    store.dispatch(new GetTokenSuccess(token));
    expect(store.dispatch).toHaveBeenCalledWith(new GetProjects());
    expect(component.token).toEqual(token);
  });

  it('initializes dropdown with those checked on the token', () => {
    spyOn(store, 'dispatch').and.callThrough();
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
