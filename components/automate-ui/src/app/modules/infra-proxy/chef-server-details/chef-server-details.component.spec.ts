import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { HttpErrorResponse } from '@angular/common/http';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule, FormsModule } from '@angular/forms';
import { StoreModule, Store } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { CreateOrgSuccess, CreateOrgFailure, GetOrgsSuccess } from 'app/entities/orgs/org.actions';
import { Org } from 'app/entities/orgs/org.model';
import { HttpStatus } from 'app/types/types';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { By } from '@angular/platform-browser';
import { ChefServerDetailsComponent } from './chef-server-details.component';
import { MockComponent } from 'ng2-mock-component';
import { using } from 'app/testing/spec-helpers';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

class MockTelemetryService {
  track() { }
}

describe('ChefServerDetailsComponent', () => {
  let component: ChefServerDetailsComponent;
  let fixture: ComponentFixture<ChefServerDetailsComponent>;
  let element;
  let errors = {};

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({
          selector: 'app-authorized',
          inputs: ['allOf']
        }),
        MockComponent({
          selector: 'app-create-org-modal',
          inputs: ['visible', 'creating', 'conflictErrorEvent', 'createForm'],
          outputs: ['close', 'createClicked']
        }),
        MockComponent({ selector: 'chef-button',
          inputs: ['disabled', 'routerLink'] }),
        MockComponent({ selector: 'mat-select' }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-form-field' }),
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
        MockComponent({ selector: 'input', inputs: ['resetOrigin'] }),
        ChefServerDetailsComponent
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
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ChefServerDetailsComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('create org', () => {
    let store: Store<NgrxStateAtom>;
    const org: Org = {
      id: '1',
      name: 'new org',
      admin_user: 'new org user',
      server_id: '39cabe9d-996e-42cd-91d0-4335b2480aaf',
      projects: ['test_org_project']
    };

    beforeEach(() => {
      store = TestBed.inject(Store);
    });


    describe('org list', () => {
      const availableOrgs: Org[] = [
        {
        id: '1',
        name: 'new org',
        admin_user: 'new org user',
        server_id: '39cabe9d-996e-42cd-91d0-4335b2480aaf',
        projects: ['test_org_project']
        }
      ];
      const emptyOrgs: Org[] = [];

      it('render the Orgs list', () => {
        store.dispatch(new GetOrgsSuccess({orgs: availableOrgs}));
        expect(component.orgs.length).not.toBeNull();
        expect(element.query(By.css('.empty-section'))).toBeNull();
      });

      it('show no preview image', () => {
        store.dispatch(new GetOrgsSuccess({orgs: emptyOrgs}));
        expect(component.orgs.length).toBe(0);
      });
    });

    it('openCreateModal opens modal', () => {
      expect(component.createModalVisible).toBe(false);
      component.openCreateModal();
      expect(component.createModalVisible).toBe(true);
    });

    it('opening create modal resets name, admin_user and admin_key to empty string', () => {
      component.openCreateModal();
      expect(component.orgForm.controls['id'].value).toEqual('');
      expect(component.orgForm.controls['name'].value).toEqual('');
      expect(component.orgForm.controls['admin_user'].value).toEqual('');
      expect(component.orgForm.controls['admin_key'].value).toEqual('');
    });

    it('on conflict error, modal remains open and displays conflict error', () => {
      spyOn(component.conflictErrorEvent, 'emit');
      component.openCreateModal();
      component.orgForm.controls['id'].setValue(org.id);
      component.orgForm.controls['name'].setValue(org.name);
      component.orgForm.controls['admin_user'].setValue(org.admin_user);
      component.orgForm.controls.projects.setValue(org.projects[0]);
      component.createServerOrg();

      const conflict = <HttpErrorResponse>{
        status: HttpStatus.CONFLICT,
        ok: false
      };
      store.dispatch(new CreateOrgFailure(conflict));

      expect(component.createModalVisible).toBe(true);
    });

    it('on success, closes modal and adds new server', () => {
      spyOn(component.conflictErrorEvent, 'emit');
      component.openCreateModal();
      component.orgForm.controls['id'].setValue(org.id);
      component.orgForm.controls['name'].setValue(org.name);
      component.orgForm.controls['admin_user'].setValue(org.admin_user);
      component.orgForm.controls.projects.setValue(org.projects[0]);
      component.createServerOrg();

      store.dispatch(new CreateOrgSuccess({'org': org}));
      expect(component.createModalVisible).toBe(true);
    });

    it('on create error, modal is closed (because error is handled by failure banner)', () => {
      spyOn(component.conflictErrorEvent, 'emit');
      component.openCreateModal();
      component.orgForm.controls['id'].setValue(org.id);
      component.orgForm.controls['name'].setValue(org.name);
      component.orgForm.controls['admin_user'].setValue(org.admin_user);
      component.orgForm.controls.projects.setValue(org.projects[0]);
      component.createServerOrg();

      const error = <HttpErrorResponse>{
        status: HttpStatus.INTERNAL_SERVER_ERROR,
        ok: false
      };

      store.dispatch(new CreateOrgFailure(error));

      expect(component.createModalVisible).toBe(false);
    });
  });

  describe('Updating server details', () => {
    beforeEach(() => {
      component.server = {
        id: '1',
        name: 'test server',
        fqdn: 'chef.party',
        ip_address: '22.24.65.65'
      };
    });

    const VALID_NAME = 'test server';
    const VALID_FQDN = 'chef.internal';
    const VALID_IP = '1.2.3.4';

    it('when all details are valid, it should update successfully', () => {
      component.updateServerForm.controls['name'].setValue(VALID_NAME);
      component.fqdnForm.controls['fqdn'].setValue(VALID_FQDN);
      component.ipForm.controls['ip_address'].setValue(VALID_IP);

      expect(component.updateServerForm.valid).toBeTrue();
      component.saveServer();
    });

    describe('form should be invalid when using invalid fqdn', () => {
      using([
        ['is using something other than http or https', 'httpld://www.chef.io'],
        ['contains two periods', 'chef..internal'],
        ['there is no TLD suffix', 'http://foo.com.'],
        ['contains hyphens in the TLD', 'chef.this-will-not'],
        ['has a TLD that is longer than 25 characters', 'chef.thisisareallylongtldandwontwork'],
        ['has a TLD that is shorter than 2 characters', 'chef.i'],
        ['has numbers in the TLD', 'chef.017'],
        ['has a port number that is too high', 'https://chef.io:987274892'],
        ['has a colon but no port number', 'https://chef.io:'],
        ['has a letter in the port', 'https://chef.io:123a'],
        ['has no domain', 'http://'],
        ['has no secure domain', 'https://'],
        ['domain is dots', 'https://..'],
        ['domain is hash', 'http://#'],
        ['domain has a space', 'http:// shouldfail.net'],
        ['contains all numbers', 'http://10.1.1.0']
      ], function (description: string, input: string) {
        it(('when the fqdn ' + description), () => {
          component.updateServerForm.controls['name'].setValue('test');
          component.ipForm.controls['ip_address'].setValue('1.2.3.4');

          component.fqdnForm.controls['fqdn'].setValue(input);
          errors = component.fqdnForm.controls['fqdn'].errors || {};

          expect(component.fqdnForm.valid).toBeFalsy();
          expect(errors['pattern']).toBeTruthy();
        });
      });
    });

    describe('form should be invalid when using invalid ip address', () => {
      using([
        ['contains too many numbers', '1.2.3.454728'],
        ['is using two periods', '1..23.4.5'],
        ['contains spaces', '1. 23.4.5'],
        ['contains hypens', '1-.4.5'],
        ['is out of range', '289.23.4.5']
      ], function (description: string, input: string) {
        it(('when the ip address ' + description), () => {
          component.updateServerForm.controls['name'].setValue('test');
          component.fqdnForm.controls['fqdn'].setValue('chef.internal');

          component.ipForm.controls['ip_address'].setValue(input);
          errors = component.ipForm.controls['ip_address'].errors || {};

          expect(component.ipForm.valid).toBeFalsy();
          expect(errors['pattern']).toBeTruthy();
        });
      });
    });
  });
});
