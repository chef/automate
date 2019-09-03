import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { StoreModule, Store } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { apiTokenEntityReducer } from 'app/entities/api-tokens/api-token.reducer';
import { GetAllTokensSuccess } from 'app/entities/api-tokens/api-token.actions';
import { ApiTokenListComponent } from './api-token-list.component';
import { policyEntityReducer } from 'app/entities/policies/policy.reducer';
import { projectsFilterReducer } from 'app/services/projects-filter/projects-filter.reducer';

describe('ApiTokenListComponent', () => {
  let component: ApiTokenListComponent;
  let fixture: ComponentFixture<ApiTokenListComponent>;
  let element: HTMLElement;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        ReactiveFormsModule,
        RouterTestingModule,
        ChefPipesModule,
        StoreModule.forRoot({
          apiTokens: apiTokenEntityReducer,
          policies: policyEntityReducer,
          projectsFilter: projectsFilterReducer
        })
      ],
      declarations: [
        ApiTokenListComponent,
        MockComponent({ selector: 'app-settings-sidebar' }),
        MockComponent({
          selector: 'chef-toolbar',
          template: '<ng-content></ng-content>'
        }),
        MockComponent({ selector: 'app-authorized',
                        inputs: ['allOf', 'anyOf'],
                        template: '<ng-content></ng-content>' }),
        MockComponent({ selector: 'app-delete-object-modal',
                        inputs: ['default', 'visible', 'objectNoun', 'objectName'],
                        outputs: ['close', 'deleteClicked'] }),
        MockComponent({ selector: 'app-create-object-modal',
                        inputs: ['creating', 'createForm',
                                 'visible', 'objectNoun', 'conflictErrorEvent',
                                 'showProjectsDropdown', 'assignableProjects'],
                        outputs: ['close', 'createClicked'] }),
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-clipboard', inputs: ['value'] }),
        MockComponent({ selector: 'chef-control-menu' }),
        MockComponent({ selector: 'chef-heading' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-option' }),
        MockComponent({ selector: 'chef-page-header' }),
        MockComponent({ selector: 'chef-subheading' }),
        MockComponent({ selector: 'chef-table' }),
        MockComponent({ selector: 'chef-thead' }),
        MockComponent({ selector: 'chef-tbody' }),
        MockComponent({ selector: 'chef-tr' }),
        MockComponent({ selector: 'chef-th' }),
        MockComponent({ selector: 'chef-td' })
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ApiTokenListComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement.nativeElement;
    fixture.detectChanges();
  });

  it('exists', () => {
    expect(component).toBeTruthy();
  });

  describe('create modal', () => {
    it('create modal opens upon clicking create button', () => {
      expect(component.createModalVisible).toBe(false);
      (<HTMLButtonElement>(element.querySelector('#create-button'))).click();
      expect(component.createModalVisible).toBe(true);
    });

    it('opening create modal resets name, id, and projects to empty', () => {
      component.createTokenForm.controls['name'].setValue('any');
      (<HTMLButtonElement>(element.querySelector('#create-button'))).click();
      expect(component.createTokenForm.controls.name.value).toBe(null);
      expect(component.createTokenForm.controls.id.value).toBe(null);
      expect(component.createTokenForm.controls.projects.value).toBe(null);
    });
  });

  describe('sortedApiTokens$', () => {
    const base = { value: 'random', active: true, created_at: '', updated_at: '', projects: [] };
    let store: Store<NgrxStateAtom>;
    beforeEach(() => {
      store = TestBed.get(Store);
    });

    it('intermixes capitals and lowercase with lowercase first', () => {
      store.dispatch(new GetAllTokensSuccess({
        tokens: [
          { ...base, id: 'uuid-1', name: 'Viewer' },
          { ...base, id: 'uuid-2', name: 'developer' },
          { ...base, id: 'uuid-3', name: 'Developer' },
          { ...base, id: 'uuid-4', name: 'viewer' }
        ]
      }));
      component.sortedApiTokens$.subscribe(tokens => {
        expect(tokens.length).toBe(4);
        expect(tokens[0]).toEqual(jasmine.objectContaining({ name: 'developer' }));
        expect(tokens[1]).toEqual(jasmine.objectContaining({ name: 'Developer' }));
        expect(tokens[2]).toEqual(jasmine.objectContaining({ name: 'viewer' }));
        expect(tokens[3]).toEqual(jasmine.objectContaining({ name: 'Viewer' }));
      });
    });

    it('sorts by whole string before case', () => {
      store.dispatch(new GetAllTokensSuccess({
        tokens: [
          { ...base, id: 'uuid-2', name: 'developer' },
          { ...base, id: 'uuid-4', name: 'developer-Manager' },
          { ...base, id: 'uuid-5', name: 'Developer' }
        ]
      }));
      component.sortedApiTokens$.subscribe(tokens => {
        expect(tokens.length).toBe(3);
        expect(tokens[0]).toEqual(jasmine.objectContaining({ name: 'developer' }));
        expect(tokens[1]).toEqual(jasmine.objectContaining({ name: 'Developer' }));
        expect(tokens[2]).toEqual(jasmine.objectContaining({ name: 'developer-Manager' }));
      });
    });

    it('uses natural ordering', () => {
      store.dispatch(new GetAllTokensSuccess({
        tokens: [
          { ...base, id: 'uuid-1', name: 'Viewer01' },
          { ...base, id: 'uuid-2', name: 'Viewer300' },
          { ...base, id: 'uuid-3', name: 'Viewer3' },
          { ...base, id: 'uuid-4', name: 'Viewer-2' },
          { ...base, id: 'uuid-6', name: 'viewer' }
        ]
      }));
      component.sortedApiTokens$.subscribe(tokens => {
        expect(tokens.length).toBe(5);
        expect(tokens[0]).toEqual(jasmine.objectContaining({ name: 'viewer' }));
        expect(tokens[1]).toEqual(jasmine.objectContaining({ name: 'Viewer-2' }));
        expect(tokens[2]).toEqual(jasmine.objectContaining({ name: 'Viewer01' }));
        expect(tokens[3]).toEqual(jasmine.objectContaining({ name: 'Viewer3' }));
        expect(tokens[4]).toEqual(jasmine.objectContaining({ name: 'Viewer300' }));
      });
    });

  });
});
