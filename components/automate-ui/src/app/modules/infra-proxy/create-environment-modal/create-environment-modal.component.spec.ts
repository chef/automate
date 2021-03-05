import { ReactiveFormsModule, FormBuilder } from '@angular/forms';
import { StoreModule, Store } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { ComponentFixture, TestBed, waitForAsync
  } from '@angular/core/testing';
import { MockComponent } from 'ng2-mock-component';

import { CreateEnvironmentModalComponent } from './create-environment-modal.component';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';
import { Environment } from 'app/entities/environments/environment.model';
import { CreateEnvironmentSuccess, CreateEnvironmentFailure } from 'app/entities/environments/environment.action';
import { HttpErrorResponse } from '@angular/common/http';
import { HttpStatus } from 'app/types/types';
import { EventEmitter } from '@angular/core';
import { HttpClient, HttpHandler } from '@angular/common/http';


class MockTelemetryService {
  track() { }
}

describe('CreateEnvironmentModalComponent', () => {
  let component: CreateEnvironmentModalComponent;
  let fixture: ComponentFixture<CreateEnvironmentModalComponent>;

  beforeEach( waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-toolbar' }),
        MockComponent({ selector: 'app-infra-tab-change' }),
        MockComponent({ selector: 'app-infra-tab', inputs: ['active', 'disabled'] }),

        MockComponent({ selector: 'chef-modal',
          inputs: ['visible']
        }),
        CreateEnvironmentModalComponent
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
    fixture = TestBed.createComponent(CreateEnvironmentModalComponent);
    component = fixture.componentInstance;
    component.firstFormGroup = new FormBuilder().group({
      name: ['', null],
      description: ['', null]
    });
    component.thirdFormGroup = new FormBuilder().group({
      dattr: ['', null]
    });
    component.fourthFormGroup = new FormBuilder().group({
      oattr: ['', null]
    });
    component.openEvent = new EventEmitter();
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });


  describe('create environment', () => {
    let store: Store<NgrxStateAtom>;
    const environment: Environment = {
      org_id: 'chef_manage',
      server_id: 'test',
      name: 'test',
      description: 'test environment',
      cookbook_versions: {
        aix: '> 2.3.4'
      },
      default_attributes: {
        test: 'test'
      },
      override_attributes: {
        test: 'test'
      }
    };

    beforeEach(() => {
      store = TestBed.inject(Store);
    });

    it('opening creat model and defaults tab to showing section', () => {
      component.visible = true;
      expect(component.detailsTab).toBe(true);
      expect(component.constraintsTab).toBe(false);
      expect(component.defaultTab).toBe(false);
      expect(component.overrideTab).toBe(false);
    });

    it('opening create modal resets to empty string', () => {
      component.visible = true;
      expect(component.firstFormGroup.controls['name'].value).toEqual('');
      expect(component.firstFormGroup.controls['description'].value).toEqual('');
      expect(component.thirdFormGroup.controls['dattr'].value).toEqual('{}');
      expect(component.fourthFormGroup.controls['oattr'].value).toEqual('{}');
    });

    it('on conflict error, modal remains open and displays conflict error', () => {
      spyOn(component.conflictErrorEvent, 'emit');
      component.visible = true;
      component.firstFormGroup.controls['name'].setValue(environment.name);
      component.firstFormGroup.controls['description'].setValue(environment.description);
      component.constraintArray = environment.cookbook_versions;
      component.thirdFormGroup.controls['dattr'].setValue(
        JSON.stringify(environment.default_attributes));
      component.fourthFormGroup.controls['oattr'].setValue(
        JSON.stringify(environment.override_attributes));

      component.createEnvironment();

      const conflict = <HttpErrorResponse>{
        status: HttpStatus.CONFLICT,
        ok: false
      };
      store.dispatch(new CreateEnvironmentFailure(conflict));
      expect(component.visible).toBe(true);
    });

    it('on success, closes modal and adds new environment', () => {
      spyOn(component.conflictErrorEvent, 'emit');
      component.visible = true;
      component.firstFormGroup.controls['name'].setValue(environment.name);
      component.firstFormGroup.controls['description'].setValue(environment.description);
      component.constraintArray = environment.cookbook_versions;
      component.thirdFormGroup.controls['dattr'].setValue(
        JSON.stringify(environment.default_attributes));
      component.fourthFormGroup.controls['oattr'].setValue(
        JSON.stringify(environment.override_attributes));
      component.createEnvironment();

      store.dispatch(new CreateEnvironmentSuccess({environment}));
      component.closeCreateModal();
      expect(component.environmentsList.length).not.toBeNull();
    });

    it('on create error, modal is closed (because error is handled by failure banner)', () => {
      spyOn(component.conflictErrorEvent, 'emit');
      component.visible = true;
      component.firstFormGroup.controls['name'].setValue(environment.name);
      component.firstFormGroup.controls['description'].setValue(environment.description);
      component.constraintArray = environment.cookbook_versions;
      component.thirdFormGroup.controls['dattr'].setValue(
        JSON.stringify(environment.default_attributes));
      component.fourthFormGroup.controls['oattr'].setValue(
        JSON.stringify(environment.override_attributes));

      component.createEnvironment();

      const error = <HttpErrorResponse>{
        status: HttpStatus.INTERNAL_SERVER_ERROR,
        ok: false
      };

      store.dispatch(new CreateEnvironmentFailure(error));

      expect(component.creating).toBe(false);
    });

  });

});
