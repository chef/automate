import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { InfraRolesComponent } from './infra-roles.component';
import { RouterTestingModule } from '@angular/router/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MockComponent } from 'ng2-mock-component';
import { Store, StoreModule } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { By } from '@angular/platform-browser';
import { GetRolesSuccess } from 'app/entities/infra-roles/infra-role.action';
import { InfraRole } from 'app/entities/infra-roles/infra-role.model';
import { using } from 'app/testing/spec-helpers';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

class MockTelemetryService {
  track() { }
}

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
        { provide: TelemetryService, useClass: MockTelemetryService },
        FeatureFlagsService
      ],
      imports: [
        FormsModule,
        ReactiveFormsModule,
        RouterTestingModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ],
      schemas: [CUSTOM_ELEMENTS_SCHEMA]
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

  describe('#search', () => {
    describe('search shows no data', () => {
      using([
        ['contains tilde.', 'role~'],
        ['contains acute, back quote,', 'role`'],
        ['contains exclamation mark', 'role!'],
        ['contains ampersat, at', 'role@'],
        ['contains dollar sign', 'role$'],
        ['contains percent.', 'role%'],
        ['contains caret or circumflex.,', 'role^'],
        ['contains ampersand', 'role&'],
        ['contains asterisk', 'role*'],
        ['contains open or left parenthesis.', 'role('],
        ['contains close or right parenthesis,', 'role)'],
        ['contains plus', 'role+'],
        ['contains equal', 'role='],
        ['contains open brace', 'role{'],
        ['contains close brace', 'role}'],
        ['contains open bracket', 'role['],
        ['contains closed bracket', 'role]'],
        ['contains pipe', 'role|'],
        ['contains backslash', 'role\\'],
        ['contains forward slash', 'role/'],
        ['contains colon', 'role:'],
        ['contains semicolon.', 'role;'],
        ['contains quote', 'role"'],
        ['contains apostrophe', 'role\'test'],
        ['contains less than', 'role<'],
        ['contains greater than', 'role>'],
        ['contains comma', 'role,'],
        ['contains period, dot', 'role.'],
        ['contains question mark', 'role?'],
        ['contains space', 'role test1'],
        ['has mixed alphabet, number, special character', 'role-test!+ test1']
      ], function (description: string, input: string) {
        it(('when the role' + description), () => {
          component.searchRoles(input);
          expect(component.roles.length).toBe(0);
          expect(component.total).toBe(0);
        });
      });

      using([
        ['contains tilde.', '~'],
        ['contains acute, back quote,', '`'],
        ['contains exclamation mark', '!'],
        ['contains ampersat, at', '@'],
        ['contains dollar sign', '$'],
        ['contains percent.', '%'],
        ['contains caret or circumflex.,', '^'],
        ['contains ampersand', '&'],
        ['contains asterisk', '*'],
        ['contains open or left parenthesis.', '('],
        ['contains close or right parenthesis,', ')'],
        ['contains plus', '+'],
        ['contains equal', '='],
        ['contains open brace', '{'],
        ['contains close brace', '}'],
        ['contains open bracket', '['],
        ['contains closed bracket', ']'],
        ['contains pipe', '|'],
        ['contains backslash', '\\'],
        ['contains forward slash', '/'],
        ['contains colon', ':'],
        ['contains semicolon.', ';'],
        ['contains quote', '"'],
        ['contains apostrophe', '\'test'],
        ['contains less than', '<'],
        ['contains greater than', '>'],
        ['contains comma', ','],
        ['contains period, dot', '.'],
        ['contains question mark', '?'],
        ['contains space', '    test1']
      ], function (description: string, input: string) {
        it(('when the role only' + description), () => {
          component.searchRoles(input);
          expect(component.roles.length).toBe(0);
          expect(component.total).toBe(0);
        });
      });
    });

    describe('search shows the role list if available', () => {
      using([
        ['contains numbers range 0-9.', 'role123'],
        ['contains alphabets a-z', 'role-test'],
        ['contains underscore.', 'role_test'],
        ['contains hyphen, minus, or dash.', 'role_test-1'],
        ['has mixed characters', 'role-Test_10']
      ], function (description: string, input: string) {
        it(('when the role ' + description), () => {
          component.searchRoles(input);
          expect(component.roles.length).not.toBeNull();
          expect(element.query(By.css('.empty-section'))).toBeNull();
        });
      });
    });
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
      run_list: []
    }];
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


