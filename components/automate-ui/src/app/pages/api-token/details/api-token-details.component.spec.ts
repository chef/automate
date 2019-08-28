import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { Router } from '@angular/router';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { routerReducer } from '@ngrx/router-store';
import { MockComponent } from 'ng2-mock-component';
import { StoreModule, Store } from '@ngrx/store';

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
import { GetIamVersionSuccess } from 'app/entities/policies/policy.actions';
import { IamVersionResponse } from 'app/entities/policies/policy.requests';
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

  let someToken: ApiToken = {
    id: targetId,
    name: 'some token',
    value: 'random',
    active: true,
    created_at: '',
    updated_at: '',
    projects: []
  };

  beforeEach(() => {
    router = TestBed.get(Router);
    spyOn(router, 'navigate').and.stub();
    store = TestBed.get(Store);

    fixture = TestBed.createComponent(ApiTokenDetailsComponent);
    component = fixture.componentInstance;
    component.token = someToken;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });

  it('initializes dropdown with those included on the token checked', () => {
    spyOn(store, 'dispatch').and.callThrough();
    someToken.projects  = ['b-proj', 'd-proj'];
    store.dispatch(new GetTokenSuccess(someToken));

    const version: IamVersionResponse = { version: { major: 'V2', minor: 'V1' } };
    store.dispatch(new GetIamVersionSuccess(version));
    expect(store.dispatch).toHaveBeenCalledWith(new GetProjects());

    const projectList = [
      genProject('a-proj'),
      genProject('b-proj'),
      genProject('c-proj'),
      genProject('d-proj')
    ];
    store.dispatch(new GetProjectsSuccess({ projects: projectList }));

    projectList.forEach(p => {
      expect(component.projects[p.id].checked).toEqual(someToken.projects.includes(p.id));
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
