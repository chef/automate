import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { Router } from '@angular/router';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule, FormsModule } from '@angular/forms';
import { StoreModule } from '@ngrx/store';
import { ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { EnvironmentDetailsComponent } from './environment-details.component';
import { MockComponent } from 'ng2-mock-component';
import { MockChefHeading, MockChefIcon, MockChefLoadingSpinner, MockChefPageHeader, MockChefSubheading, MockChefTabSelector, MockChefTable, MockChefTbody, MockChefTd, MockChefTh, MockChefThead, MockChefToolbar, MockChefTr } from 'app/testing/mock-components';
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

describe('EnvironmentDetailsComponent', () => {
  let router: Router;
  let component: EnvironmentDetailsComponent;
  let fixture: ComponentFixture<EnvironmentDetailsComponent>;
  let element;
  const role = {
      'name': 'starter',
      'chef_type': 'role',
      'description': 'An example Chef role',
      'default_attributes': '{"my-cookbook": {"port": 80, "code": {"location": "github"}}}',
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
      imports: [
        FormsModule,
        ReactiveFormsModule,
        RouterTestingModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks }),
        MockComponent({ selector: 'a', inputs: ['routerLink'] }),
        MockChefHeading,
        MockChefIcon,
        MockChefLoadingSpinner,
        MockChefPageHeader,
        MockChefSubheading,
        MockChefTabSelector,
        MockChefToolbar,
        MockChefTable,
        MockChefThead,
        MockChefTbody,
        MockChefTr,
        MockChefTh,
        MockChefTd,
        MockComponent({ selector: 'input', inputs: ['resetOrigin'] }),
        MockComponent({ selector: 'mat-select' }),
        MockComponent({ selector: 'mat-option' })
      ],
      declarations: [
        EnvironmentDetailsComponent,
        JsonTreeTable
      ],
      providers: [
        FeatureFlagsService,
        MockAttributesService
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(EnvironmentDetailsComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement;
    fixture.detectChanges();
    router = TestBed.inject(Router);
    component.attributes = new RoleAttributes(role);
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });

  it('defaults to showing cookbook_constraints section', () => {
    expect(component.tabValue).toBe('cookbookConstraints');
  });

  it('shows/hides sections when based on selection', () => {
    spyOn(router, 'navigate');

    component.onSelectedTab({ target: { value: 'cookbookConstraints' } });
    expect(component.tabValue).toBe('cookbookConstraints');
    expect(router.navigate).toHaveBeenCalled();

    component.onSelectedTab({ target: { value: 'attributes' } });
    expect(component.tabValue).toBe('attributes');
    expect(router.navigate).toHaveBeenCalled();
  });

  describe('AttributesComponent', () => {
    it('renders the attributes component correctly', () => {
      expect(element.query(By.css('.jsontree_value_object'))).toBeNull();
    });

    it('fetches attributes and returns an EnvironmentAttributes object', waitForAsync(() => {
      fixture.whenStable().then(() => {
        expect(component.retrieve('default_attributes')).toEqual(retrieve_default);
      });
    }));
  });

});
