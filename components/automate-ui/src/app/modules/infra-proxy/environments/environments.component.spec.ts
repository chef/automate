import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';

import { EnvironmentsComponent } from './environments.component';
import { RouterTestingModule } from '@angular/router/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MockComponent } from 'ng2-mock-component';
import { Store, StoreModule } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { By } from '@angular/platform-browser';
import { GetEnvironmentsSuccess } from 'app/entities/environments/environment.action';
import { Environment } from 'app/entities/environments/environment.model';

describe('EnvironmentsComponent', () => {
  let component: EnvironmentsComponent;
  let fixture: ComponentFixture<EnvironmentsComponent>;
  let element;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'chef-th' }),
        MockComponent({ selector: 'chef-td' }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-heading' }),
        MockComponent({ selector: 'chef-icon' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'mat-select' }),
        MockComponent({ selector: 'mat-option' }),
        MockComponent({ selector: 'chef-page-header' }),
        MockComponent({ selector: 'chef-subheading' }),
        MockComponent({ selector: 'chef-toolbar' }),
        MockComponent({ selector: 'chef-table' }),
        MockComponent({ selector: 'chef-thead' }),
        MockComponent({ selector: 'chef-tbody' }),
        MockComponent({ selector: 'chef-tr' }),
        MockComponent({ selector: 'chef-th' }),
        MockComponent({ selector: 'chef-td' }),
        MockComponent({ selector: 'a', inputs: ['routerLink'] }),
        MockComponent({ selector: 'input', inputs: ['resetOrigin'] }),
        MockComponent({
          selector: 'app-delete-infra-object-modal',
          inputs: ['default', 'visible', 'objectNoun', 'objectName'],
          outputs: ['close', 'deleteClicked']
        }),
        EnvironmentsComponent
      ],
      providers: [
        FeatureFlagsService
      ],
      imports: [
        FormsModule,
        ReactiveFormsModule,
        RouterTestingModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(EnvironmentsComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('environment list', () => {
    let store: Store<NgrxStateAtom>;
    const availableEnvironments: Environment[] = [{
      server_id: 'test_server_id',
      org_id: 'test_org_id',
      name: 'test4',
      description: 'test override',
      cookbook_versions: [],
      json_class: 'Chef::Environment',
      chef_type: 'environment',
      default_attributes: 'test',
      override_attributes:  'test',
      run_list: []
    }];

    const emptyEnvironments: Environment[] = [];

    beforeEach(() => {
      store = TestBed.inject(Store);
    });

    it('render the environment list', () => {
      store.dispatch(
        new GetEnvironmentsSuccess({
        environments: availableEnvironments, total: availableEnvironments.length
      }));
      expect(component.environments.length).not.toBeNull();
      expect(element.query(By.css('.empty-section'))).toBeNull();
    });

    it('show no preview image', () => {
      store.dispatch(
        new GetEnvironmentsSuccess({
          environments: emptyEnvironments, total: emptyEnvironments.length
        }));
      expect(component.environments.length).toBe(0);
    });
  });
});
