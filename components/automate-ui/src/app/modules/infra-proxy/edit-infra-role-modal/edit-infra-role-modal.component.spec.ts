import { ReactiveFormsModule } from '@angular/forms';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { MockComponent } from 'ng2-mock-component';
import { StoreModule } from '@ngrx/store';
import { ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { EventEmitter } from '@angular/core';
import { EditInfraRoleModalComponent } from './edit-infra-role-modal.component';
import { HttpClient, HttpHandler } from '@angular/common/http';
import { InfraRole } from 'app/entities/infra-roles/infra-role.model';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

class MockTelemetryService {
  track() { }
}

describe('EditInfraRoleModalComponent', () => {
  let component: EditInfraRoleModalComponent;
  let fixture: ComponentFixture<EditInfraRoleModalComponent>;

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
        EditInfraRoleModalComponent
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
    fixture = TestBed.createComponent(EditInfraRoleModalComponent);
    component = fixture.componentInstance;
    component.openEvent = new EventEmitter();
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('#UpdateRole', () => {
    const role: InfraRole = {
      org_id: 'chef_manage',
      server_id: 'test',
      name: 'test',
      description: 'test role',
      run_list: ['recipe[aix::nim_master_setup_standalone]', 'recipe[audit]'],
      default_attributes: '{test:test}',
      override_attributes: '{test:test}'
    };
    const invalidJson = '{"invalid "test"';

    it('default attribute should be invalid when empty', () => {
      component.defaultAttributeForm.controls['default'].setValue(' ');
      expect(component.defaultAttributeForm.controls['default'].invalid).toBeFalsy();
    });

    it('default attribute should be valid when json data is filled out', () => {
      component.defaultAttributeForm.controls['default'].setValue(role.default_attributes);
      expect(component.defaultAttributeForm.valid).toBeTruthy();
    });

    it('default attribute show error when invalid json data is filled out', () => {
      component.defaultAttributeForm.controls['default'].setValue(invalidJson);
      component.onChangeDefaultJson({ target: { value: invalidJson}});
      expect(component.defaultAttrParseError).toBe(true);
    });

    it('override attribute should be invalid when empty', () => {
      component.overrideAttributeForm.controls['override'].setValue(' ');
      expect(component.overrideAttributeForm.controls['override'].invalid).toBeFalsy();
    });

    it('override attribute should be valid when json data is filled out', () => {
      component.overrideAttributeForm.controls['override'].setValue(
        role.override_attributes);
      expect(component.overrideAttributeForm.valid).toBeTruthy();
    });

    it('override attribute show error when invalid json data is filled out', () => {
      component.overrideAttributeForm.controls['override'].setValue(invalidJson);
      component.onChangeOverrideJson({ target: { value: invalidJson}});
      expect(component.overrideAttrParseError).toBe(true);
    });
  });
});
