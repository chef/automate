import { ReactiveFormsModule } from '@angular/forms';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { MockComponent } from 'ng2-mock-component';
import { StoreModule } from '@ngrx/store';
import { ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { EventEmitter } from '@angular/core';
import { EditInfraRoleModalComponent } from './edit-infra-role-modal.component';
import { HttpClient, HttpHandler } from '@angular/common/http';
import { InfraRole } from 'app/entities/infra-roles/infra-role.model';

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

    it('default attribute should be valid when json data is filled out', () => {
      component.defaultAttributeForm.controls['default'].setValue(role.default_attributes);
      expect(component.defaultAttributeForm.valid).toBeTruthy();
    });

    it('override attribute should be valid when json data is filled out', () => {
      component.overrideAttributeForm.controls['override'].setValue(
        role.override_attributes);
      expect(component.overrideAttributeForm.valid).toBeTruthy();
    });
  });
});
