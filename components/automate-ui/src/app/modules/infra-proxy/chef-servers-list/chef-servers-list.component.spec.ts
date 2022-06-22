import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { HttpErrorResponse } from '@angular/common/http';
import { RouterTestingModule } from '@angular/router/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatOptionSelectionChange } from '@angular/material/core/option';
import { MockComponent } from 'ng2-mock-component';
import { StoreModule, Store } from '@ngrx/store';

import { ChefServersListComponent } from './chef-servers-list.component';
import { Server } from 'app/entities/servers/server.model';
import { CreateServerSuccess, CreateServerFailure, GetServersSuccess } from 'app/entities/servers/server.actions';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { HttpStatus } from 'app/types/types';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { By } from '@angular/platform-browser';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

class MockTelemetryService {
  track() { }
}

describe('ChefServersListComponent', () => {
  let component: ChefServersListComponent;
  let fixture: ComponentFixture<ChefServersListComponent>;
  let element;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({
          selector: 'app-authorized',
          inputs: ['allOf']
        }),
        MockComponent({
          selector: 'app-create-chef-server-modal',
          inputs: ['visible', 'creating', 'conflictErrorEvent', 'createForm'],
          outputs: ['close', 'createClicked']
        }),
        MockComponent({
          selector: 'app-delete-object-modal',
          inputs: ['default', 'visible', 'objectNoun', 'objectName'],
          outputs: ['close', 'deleteClicked']
        }),
        MockComponent({
          selector: 'chef-button',
          inputs: ['disabled', 'routerLink']
        }),
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
        ChefServersListComponent
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
    fixture = TestBed.createComponent(ChefServersListComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('server list', () => {
    let store: Store<NgrxStateAtom>;
    const availableServers: Server[] = [
      {
        id: '1',
        name: 'new server',
        fqdn: 'xyz.com',
        ip_address: '1.1.1.1'
      }
    ];
    const emptyServers: Server[] = [];

    beforeEach(() => {
      store = TestBed.inject(Store);
    });

    it('render the server list', () => {
      store.dispatch(new GetServersSuccess({servers: availableServers}));
      expect(component.servers.length).not.toBeNull();
      expect(element.query(By.css('.empty-section'))).toBeNull();
    });

    it('show no preview image', () => {
      store.dispatch(new GetServersSuccess({servers: emptyServers}));
      expect(component.servers.length).toBe(0);
    });
  });

  describe('create server', () => {
    let store: Store<NgrxStateAtom>;
    const server = <Server>{
      id: '1',
      name: 'new server',
      fqdn: 'xyz.com',
      ip_address: '1.1.1.1'
    };

    beforeEach(() => {
      store = TestBed.inject(Store);
    });

    it('openCreateModal opens modal', () => {
      expect(component.createModalVisible).toBe(false);
      component.openCreateModal();
      expect(component.createModalVisible).toBe(true);
    });

    it('opening create modal resets id, name, fqdn and ip_address to empty string', () => {
      component.createChefServerForm.controls['id'].setValue('any');
      component.createChefServerForm.controls['name'].setValue('any');
      component.fqdnForm.controls['fqdn'].setValue('any');
      component.ipForm.controls['ip_address'].setValue('any');
      component.openCreateModal();
      expect(component.createChefServerForm.controls['id'].value).toBe(null);
      expect(component.createChefServerForm.controls['name'].value).toBe(null);
      expect(component.fqdnForm.controls['fqdn'].value).toBe(null);
      expect(component.ipForm.controls['ip_address'].value).toBe(null);
    });

    it('on success, closes modal and adds new server', () => {
      component.createChefServerForm.controls['id'].setValue(server.id);
      component.createChefServerForm.controls['name'].setValue(server.name);
      component.fqdnForm.controls['fqdn'].setValue(server.fqdn);
      component.ipForm.controls['ip_address'].setValue(server.ip_address);
      component.createChefServer();

      store.dispatch(new CreateServerSuccess({ 'server': server }));
      expect(component.creatingChefServer).toBe(true);
      expect(component.createModalVisible).toBe(false);
    });

    it('on conflict error, modal is open with conflict error', () => {
      spyOn(component.conflictErrorEvent, 'emit');
      component.openCreateModal();
      component.createChefServerForm.controls['id'].setValue(server.id);
      component.createChefServerForm.controls['name'].setValue(server.name);
      component.fqdnForm.controls['fqdn'].setValue(server.fqdn);
      component.ipForm.controls['ip_address'].setValue(server.ip_address);
      component.createChefServer();

      const conflict = <HttpErrorResponse>{
        status: HttpStatus.CONFLICT,
        ok: false
      };
      store.dispatch(new CreateServerFailure(conflict));

      expect(component.createModalVisible).toBe(true);
      expect(component.conflictErrorEvent.emit).toHaveBeenCalled();
    });

    it('on create error, modal is closed with failure banner', () => {
      spyOn(component.conflictErrorEvent, 'emit');
      component.openCreateModal();
      component.createChefServerForm.controls['id'].setValue(server.id);
      component.createChefServerForm.controls['name'].setValue(server.name);
      component.fqdnForm.controls['fqdn'].setValue(server.fqdn);
      component.ipForm.controls['ip_address'].setValue(server.ip_address);
      component.createChefServer();

      const error = <HttpErrorResponse>{
        status: HttpStatus.INTERNAL_SERVER_ERROR,
        ok: false
      };
      store.dispatch(new CreateServerFailure(error));

      expect(component.createModalVisible).toBe(false);
      expect(component.conflictErrorEvent.emit).toHaveBeenCalledWith(false);
    });

  });

  describe('create server form validation', () => {

    it('- fqdn field validity', () => {
      component.openCreateModal();

      let errors = {};
      const fqdn = component.fqdnForm.controls['fqdn'];
      expect(fqdn.valid).toBeFalsy();

      // FQDN field is required
      errors = fqdn.errors || {};
      expect(errors['required']).toBeTruthy();

      // Set fqdn to invalid inputs
      fqdn.setValue('http://foo.bar-.-.');
      errors = fqdn.errors || {};
      expect(errors['pattern']).toBeTruthy();

      fqdn.setValue('http://foo.bar..com/');
      errors = fqdn.errors || {};
      expect(errors['pattern']).toBeTruthy();

      fqdn.setValue('http://...foo.bar.com/');
      errors = fqdn.errors || {};
      expect(errors['pattern']).toBeTruthy();

      fqdn.setValue('http://...foo.bar com/');
      errors = fqdn.errors || {};
      expect(errors['pattern']).toBeTruthy();

      // Set fqdn to valid inputs
      fqdn.setValue('http://fo_o.bar/');
      errors = fqdn.errors || {};
      expect(errors['pattern']).toBeFalsy();

      fqdn.setValue('https://bit.ly/2OWGwiL');
      errors = fqdn.errors || {};
      expect(errors['pattern']).toBeFalsy();

      fqdn.setValue('http://demo.com/');
      errors = fqdn.errors || {};
      expect(errors['pattern']).toBeFalsy();

      fqdn.setValue('https://example_test.co.in/2OWGwiL');
      errors = fqdn.errors || {};
      expect(errors['pattern']).toBeFalsy();

    });
  });

  describe('delete modal', () => {
    const mockEvent = { isUserInput: true } as MatOptionSelectionChange;

    it('With no orgs, selecting delete from control menu opens the delete modal', () => {
      expect(component.deleteModalVisible).toBe(false);
      component.startServerDelete(mockEvent, genServer('uuid-111', 0));
      expect(component.deleteModalVisible).toBe(true);
    });

    it('closes upon sending request to back-end', () => {
      component.startServerDelete(mockEvent, genServer('uuid-111', 0));
      expect(component.deleteModalVisible).toBe(true);
      component.deleteServer();
      expect(component.deleteModalVisible).toBe(false);
    });

  });

  describe('message modal', () => {
    const mockEvent = { isUserInput: true } as MatOptionSelectionChange;

    it('With one org, selecting delete from control menu opens the message modal', () => {
      expect(component.messageModalVisible).toBe(false);
      component.startServerDelete(mockEvent, genServer('uuid-111', 1));
      expect(component.messageModalVisible).toBe(true);
    });

    it('With multiple orgs, selecting delete from control menu opens the message modal', () => {
      expect(component.messageModalVisible).toBe(false);
      component.startServerDelete(mockEvent, genServer('uuid-111', 1));
      expect(component.messageModalVisible).toBe(true);
    });

    it('closes upon request', () => {
      component.startServerDelete(mockEvent, genServer('uuid-111', 4));
      expect(component.messageModalVisible).toBe(true);
      component.closeMessageModal();
      expect(component.messageModalVisible).toBe(false);
    });

  });

  function genServer(id: string, orgs_count: number): Server {
    return {
      id,
      orgs_count,
      name: 'Demo Server',
      fqdn: 'http://demo.com/',
      ip_address: '192.168.2.1'
    };
  }

});
