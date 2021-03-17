import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { InfraRolesComponent } from './infra-roles.component';
import { RouterTestingModule } from '@angular/router/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MockComponent } from 'ng2-mock-component';
import { Store, StoreModule } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { By } from '@angular/platform-browser';
import {  GetRolesSuccess } from 'app/entities/infra-roles/infra-role.action';
import { InfraRole } from 'app/entities/infra-roles/infra-role.model';

describe('InfraRolesComponent', () => {
  let component: InfraRolesComponent;
  let fixture: ComponentFixture<InfraRolesComponent>;
  let element;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'a', inputs: ['routerLink'] }),
        MockComponent({ selector: 'app-create-infra-role-modal',
        inputs: ['openEvent', 'rolesList', 'serverId', 'orgId', 'currentPage'] }),
        MockComponent({ selector: 'app-delete-infra-object-modal',
        inputs: ['visible', 'objectNoun', 'objectAction', 'custom', 'objectName'],
        outputs: ['close', 'deleteClicked'] }),
        MockComponent({ selector: 'chef-heading' }),
        MockComponent({ selector: 'chef-icon' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-page-header' }),
        MockComponent({ selector: 'chef-subheading' }),
        MockComponent({ selector: 'chef-toolbar' }),
        MockComponent({ selector: 'chef-table' }),
        MockComponent({ selector: 'chef-thead' }),
        MockComponent({ selector: 'chef-tbody' }),
        MockComponent({ selector: 'chef-tr' }),
        MockComponent({ selector: 'chef-th' }),
        MockComponent({ selector: 'chef-td' }),
        MockComponent({ selector: 'input', inputs: ['resetOrigin'] }),
        MockComponent({ selector: 'mat-select' }),
        MockComponent({ selector: 'mat-option' }),
        InfraRolesComponent
      ],
      providers: [
        FeatureFlagsService
      ],
      imports: [
        FormsModule,
        ReactiveFormsModule,
        RouterTestingModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(InfraRolesComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('infra role list', () => {
    let store: Store<NgrxStateAtom>;
    const availableRoles: InfraRole[] = [{
      server_id: 'test_server_id',
      org_id: 'test_org_id',
      name: 'test',
      environments: [],
      description: 'test role',
      json_class: 'Chef::Environment',
      chef_type: 'environment',
      default_attributes: 'test',
      override_attributes:  'test',
      run_list: [],
      expanded_run_list: []
    }
    ];
    const emptyRoles: InfraRole[] = [];

    beforeEach(() => {
      store = TestBed.inject(Store);
    });

    it('render the roles list', () => {
      store.dispatch(new GetRolesSuccess({roles: availableRoles,  total: availableRoles.length}));
      expect(component.roles.length).not.toBeNull();
      expect(element.query(By.css('.empty-section'))).toBeNull();
    });

    it('show no preview image', () => {
      store.dispatch(new GetRolesSuccess({roles: emptyRoles,  total: emptyRoles.length}));
      expect(component.roles.length).toBe(0);
    });
  });
});


