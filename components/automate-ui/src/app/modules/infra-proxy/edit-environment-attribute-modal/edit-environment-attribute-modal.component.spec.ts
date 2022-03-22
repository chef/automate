import { ReactiveFormsModule } from '@angular/forms';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { MockComponent } from 'ng2-mock-component';
import { StoreModule } from '@ngrx/store';
import { ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { EventEmitter } from '@angular/core';
import { EditEnvironmentAttributeModalComponent } from './edit-environment-attribute-modal.component';
import { HttpClient, HttpHandler } from '@angular/common/http';
import { Environment } from 'app/entities/environments/environment.model';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

class MockTelemetryService {
  track() { }
}

describe('EditEnvironmentAttributeModalComponent', () => {
  let component: EditEnvironmentAttributeModalComponent;
  let fixture: ComponentFixture<EditEnvironmentAttributeModalComponent>;

  beforeEach( waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-toolbar' }),
        MockComponent({ selector: 'chef-modal',
          inputs: ['visible']
        }),
        EditEnvironmentAttributeModalComponent
      ],
      providers: [
        { provide: TelemetryService, useClass: MockTelemetryService },
        HttpClient, HttpHandler
      ],
      imports: [
        ReactiveFormsModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(EditEnvironmentAttributeModalComponent);
    component = fixture.componentInstance;
    component.openEvent = new EventEmitter();
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('#UpdateEnvironment', () => {
    const environment: Environment = {
      org_id: 'chef_manage',
      server_id: 'test',
      name: 'test',
      description: 'test environment',
      cookbook_versions: [{
        id: 1,
        name: 'aix',
        version: '2.3.4',
        operator: '<'
      }],
      default_attributes: '{test:test}',
      override_attributes: '{test:test}',
      chef_type: 'environment',
      json_class: 'Chef::Environment'
    };

    it('default attribute should be valid when json data is filled out', () => {
      component.defaultAttributeForm.controls['default'].setValue(environment.default_attributes);
      expect(component.defaultAttributeForm.valid).toBeTruthy();
    });

    it('override attribute should be valid when json data is filled out', () => {
      component.overrideAttributeForm.controls['override'].setValue(
        environment.override_attributes);
      expect(component.overrideAttributeForm.valid).toBeTruthy();
    });
  });
});
