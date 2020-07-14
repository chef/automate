import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { TestBed, ComponentFixture } from '@angular/core/testing';
import { ServicesSidebarComponent } from './services-sidebar.component';
import { StoreModule, Store } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import {
  GroupService
} from '../../entities/service-groups/service-groups.model';

import { TelemetryService } from 'app/services/telemetry/telemetry.service';

class MockTelemetryService {
  track(_event?: string, _properties?: any): void { }
}

describe('ServicesSidebarComponent', () => {
  let component: ServicesSidebarComponent;
  let fixture: ComponentFixture<ServicesSidebarComponent>;
  let store: Store<NgrxStateAtom>;
  let mockServices: GroupService[];

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ],
      declarations: [
        ServicesSidebarComponent
      ],
      providers: [
        { provide: TelemetryService, useClass: MockTelemetryService },
      ],
      schemas: [CUSTOM_ELEMENTS_SCHEMA]
    });

    fixture = TestBed.createComponent(ServicesSidebarComponent);
    component = fixture.componentInstance;
    store = TestBed.inject(Store);
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('when selecting and deselecting checkboxes', () => {
    beforeEach(() => {
      mockServices = [
      {
        id: 300,
        supervisor_id: '1af6ecdc-acdd-421c-94f7-ec4f8c128691',
        release: 'custom/nginx/1.0.1/20190115184823',
        group: 'nginx.default',
        status: 'ok',
        health_check: 'CRITICAL',
        applications: 'bldr-cache',
        environment: 'qa',
        fqdn: '8ouxxga6.example.net',
        site: 'testsite',
        channel: 'stable',
        update_strategy: 'AT-ONCE',
        previous_health_check: 'NONE',
        current_health_since: '23 hours 54 minutes',
        health_updated_at: new Date(),
        health_check_result: {
            stdout: 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF…FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF', stderr: 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF…FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF', exit_status: 2
          }
        },
        {
          id: 400,
          supervisor_id: '1af6ecdc-acdd-421c-94f7-ec4f8c128691',
          release: 'custom/nginx/1.0.1/20190115184823',
          group: 'nginx.default',
          status: 'ok',
          health_check: 'CRITICAL',
          applications: 'bldr-cache',
          environment: 'qa',
          fqdn: '8ouxxga6.example.net',
          site: 'testsite',
          channel: 'stable',
          update_strategy: 'AT-ONCE',
          previous_health_check: 'NONE',
          current_health_since: '23 hours 54 minutes',
          health_updated_at: new Date(),
          health_check_result: {
            stdout: 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF…FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF', stderr: 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF…FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF', exit_status: 2
          }
        },
        {
          id: 500,
          supervisor_id: '1af6ecdc-acdd-421c-94f7-ec4f8c128691',
          release: 'custom/nginx/1.0.1/20190115184823',
          group: 'nginx.default',
          status: 'ok',
          health_check: 'CRITICAL',
          applications: 'bldr-cache',
          environment: 'qa',
          fqdn: '8ouxxga6.example.net',
          site: 'testsite',
          channel: 'stable',
          update_strategy: 'AT-ONCE',
          previous_health_check: 'NONE',
          current_health_since: '23 hours 54 minutes',
          health_updated_at: new Date(),
          health_check_result: {
            stdout: 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF…FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF', stderr: 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF…FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF', exit_status: 2
          }
        }
      ];

      component.servicesList = mockServices;
    });

    it('should add the id of the selected service to a list', () => {
      expect(component.checkedServices.length).toEqual(0);

      component.handleToggleCheckbox(300, true);
      component.handleToggleCheckbox(400, true);

      expect(component.checkedServices.length).toEqual(2);
      expect(component.checkedServices).toContain(300 && 400);
    });

    it('should remove already selected services from the list', () => {
      component.checkedServices = [300, 400, 500];
      expect(component.checkedServices.length).toEqual(3);

      component.handleToggleCheckbox(300, false);
      expect(component.checkedServices.length).toEqual(2);
      expect(component.checkedServices).not.toContain(300);

      component.handleToggleCheckbox(500, false);
      expect(component.checkedServices.length).toEqual(1);
      expect(component.checkedServices).not.toContain(300 && 500);
    });

    describe('Selecting or deselecting all at once', () => {
      it('when checked, should add all service ids to the checkedServices list', () => {
        expect(component.checkedServices.length).toEqual(0);

        component.handleSelectAll(true);
        expect(component.checkedServices.length).toEqual(3);
        expect(component.checkedServices).toContain(300 && 400 && 500);
      });

      it('when deselecting, removes all service ids from the checkedServices list', () => {
        component.checkedServices = [300, 400, 500];
        expect(component.checkedServices.length).toEqual(3);

        component.handleSelectAll(false);
        expect(component.checkedServices.length).toEqual(0);
      })
    });
  });
});
