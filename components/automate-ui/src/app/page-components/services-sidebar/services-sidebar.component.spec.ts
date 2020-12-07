import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { TestBed, ComponentFixture, waitForAsync } from '@angular/core/testing';
import { ServicesSidebarComponent } from './services-sidebar.component';
import { MockComponent } from 'ng2-mock-component';
import { StoreModule, Store } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import {
  GroupService
} from 'app/entities/service-groups/service-groups.model';

import { TelemetryService } from 'app/services/telemetry/telemetry.service';
import { By } from '@angular/platform-browser';
import { DeleteServicesById } from 'app/entities/service-groups/service-groups.actions';

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
        ServicesSidebarComponent,
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] })
      ],
      providers: [
        { provide: TelemetryService, useClass: MockTelemetryService }
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
          stdout: 'This is the stdout for 300',
          stderr: 'Here is the stderr for 300',
            exit_status: 2
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
            stdout: 'This is the stdout for 400',
            stderr: 'Here is the stderr for 400',
            exit_status: 2
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
            stdout: 'This is the stdout for 500',
            stderr: 'Here is the stderr for 500',
            exit_status: 2
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

    it('should update the count of services selected', () => {
      component.ngOnInit();
      expect(component.checkedServicesDisplay).toBe('Services');
      component.handleToggleCheckbox(300, true);
      component.handleToggleCheckbox(400, true);
      expect(component.checkedServices.length).toEqual(2);

      expect(component.checkedServicesDisplay).toBe('2 Services');

      component.handleToggleCheckbox(300, false);
      expect(component.checkedServices.length).toEqual(1);
      expect(component.checkedServicesDisplay).toBe('1 Service');
    });

    xit('should disable the Delete Services button when none are selected', () => {
      expect(component.checkedServices.length).toEqual(0);

      const deleteButton = fixture.debugElement.query(By.css('#delete-services-button'));
      expect(deleteButton).not.toBeNull();
      expect(deleteButton.nativeElement.disabled).toBeTruthy();

      component.handleToggleCheckbox(300, true);
      expect(component.checkedServices.length).toEqual(1);
      fixture.detectChanges();

      expect(deleteButton.nativeElement.disabled).not.toBeTruthy();
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
      });
    });

    describe('clicking delete services button', () => {

      it('delete modal should be visible upon clicking delete', waitForAsync(() => {
        expect(component.checkedServices.length).toEqual(0);
        component.handleToggleCheckbox(300, true);
        expect(component.checkedServices.length).toEqual(1);

        expect(component.deleteModalVisible).toBe(false);
        component.beginServicesDelete();

        expect(component.deleteModalVisible).toBe(true);
      }));

      it('should hide the delete modal when cancelled with no change to selections', () => {
        expect(component.checkedServices.length).toEqual(0);
        component.handleToggleCheckbox(300, true);
        component.handleToggleCheckbox(400, true);
        component.handleToggleCheckbox(500, true);
        expect(component.checkedServices.length).toEqual(3);

        component.beginServicesDelete();
        expect(component.deleteModalVisible).toBe(true);

        component.closeDeleteModal();
        expect(component.deleteModalVisible).toBe(false);
        expect(component.checkedServices.length).toEqual(3);
        expect(component.checkedServices).toContain(300 && 400 && 500);
      });

      it('inside delete modal, clicking Delete button should delete services', () => {
        component.handleToggleCheckbox(500, true);
        expect(component.checkedServices.length).toEqual(1);
        expect(component.checkedServices).toEqual([500]);

        spyOn(store, 'dispatch');
        component.deleteServices();

        expect(store.dispatch).toHaveBeenCalledWith(
          new DeleteServicesById({servicesToDelete: component.checkedServices})
          );
      });

    });


  });
});
