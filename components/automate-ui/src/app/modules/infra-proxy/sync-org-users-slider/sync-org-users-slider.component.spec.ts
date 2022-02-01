import { EventEmitter } from '@angular/core';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { ReactiveFormsModule, FormBuilder, Validators, FormGroup } from '@angular/forms';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { MockComponent } from 'ng2-mock-component';

import { SyncOrgUsersSliderComponent } from './sync-org-users-slider.component';

describe('SyncOrgUsersSliderComponent', () => {
  let component: SyncOrgUsersSliderComponent;
  let fixture: ComponentFixture<SyncOrgUsersSliderComponent>;

  let uploadForm: FormGroup;

  beforeEach( waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'input'}),
        MockComponent({ selector: 'chef-loading-spinner' }),
        SyncOrgUsersSliderComponent
      ],
      imports: [
        ReactiveFormsModule
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(SyncOrgUsersSliderComponent);
    component = fixture.componentInstance;

    component.uploadForm = new FormBuilder().group({
      file: ['', [Validators.required]]
    });

    component.conflictErrorEvent = new EventEmitter();
    uploadForm = component.uploadForm;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('form validity', () => {
    describe('the form should be invalid', () => {
      it('when file inputs are empty', () => {
        expect(uploadForm.valid).toBeFalsy();
      });
    });
  });
});
