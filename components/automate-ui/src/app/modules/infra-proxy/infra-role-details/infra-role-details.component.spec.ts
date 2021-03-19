import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { Router } from '@angular/router';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule, FormsModule } from '@angular/forms';
import { StoreModule } from '@ngrx/store';
import { ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { InfraRoleDetailsComponent } from './infra-role-details.component';
import { MockComponent } from 'ng2-mock-component';
import { JsonTreeTableComponent as JsonTreeTable } from './../json-tree-table/json-tree-table.component';
import { RoleAttributes } from 'app/entities/infra-roles/infra-role.model';

class MockAttributesService {
  nullRoleAttributes = new RoleAttributes({
    default_attributes: '',
    override_attributes: ''
  });

  fetch() {
    return Promise.resolve(
      new RoleAttributes({
        default_attributes: '{"my-cookbook": {"port": 80, "code": {"location": "github"}}}',
        override_attributes: '{}'
      })
    );
  }
}

describe('InfraRoleDetailsComponent', () => {
  let router: Router;
  let component: InfraRoleDetailsComponent;
  let fixture: ComponentFixture<InfraRoleDetailsComponent>;
  let element;
  const role = {
      'name': 'starter',
      'chef_type': 'role',
      'description': 'An example Chef role',
      'default_attributes': '{\"my-cookbook\": {\"port\": 80, \"code\": {\"location\": \"github\"}}}',
      'override_attributes': '{}',
      'json_class': 'Chef::Role',
      'run_list': [],
      'expanded_run_list': []
    };

  const retrieve_default = {
    'my-cookbook': {
      port: 80,
      code: {
        location: 'github'
      }
    }
  };

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'input', inputs: ['resetOrigin'] }),
        MockComponent({ selector: 'chef-button',
          inputs: ['disabled', 'routerLink'] }),
        MockComponent({ selector: 'mat-select' }),
        MockComponent({ selector: 'chef-th' }),
        MockComponent({ selector: 'chef-td' }),
        MockComponent({ selector: 'chef-heading' }),
        MockComponent({ selector: 'chef-icon' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-option' }),
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
        MockComponent({ selector: 'chef-tab-selector',
          inputs: ['value', 'routerLink', 'fragment']
        }),
        InfraRoleDetailsComponent,
        JsonTreeTable
      ],
      providers: [
        FeatureFlagsService,
        MockAttributesService
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
    fixture = TestBed.createComponent(InfraRoleDetailsComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement;
    fixture.detectChanges();
    router = TestBed.inject(Router);
    component.attributes = new RoleAttributes(role);
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });

  it('defaults to showing run_list section', () => {
    expect(component.tabValue).toBe('runList');
  });

  it('shows/hides sections when based on selection', () => {
    spyOn(router, 'navigate');

    component.onSelectedTab({ target: { value: 'runList' } });
    expect(component.tabValue).toBe('runList');
    expect(router.navigate).toHaveBeenCalled();

    component.onSelectedTab({ target: { value: 'attributes' } });
    expect(component.tabValue).toBe('attributes');
    expect(router.navigate).toHaveBeenCalled();
  });

  describe('empty state', () => {
    it('run_list array should be empty', () => {
      expect(component.arrayOfNodesTree.length).toEqual(0);
    });
  });

  describe('AttributesComponent', () => {
    it('renders the attributes component correctly', () => {
      expect(element.query(By.css('.jsontree_value_object'))).toBeNull();
    });

    it('fetches attributes and returns a NodeAttributes object', waitForAsync(() => {
      fixture.whenStable().then(() => {
        expect(component.retrieve('default_attributes')).toEqual(retrieve_default);
      });
    }));
  });

  // Tree-table specs covered in './tree-table/services/tree/tree.service.spec.ts'
});
