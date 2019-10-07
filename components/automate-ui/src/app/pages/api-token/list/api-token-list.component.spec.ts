import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { StoreModule } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';

import { runtimeChecks } from 'app/ngrx.reducers';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { apiTokenEntityReducer } from 'app/entities/api-tokens/api-token.reducer';
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
        }, { runtimeChecks })
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
});
