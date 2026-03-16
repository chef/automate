import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { ReactiveFormsModule, FormBuilder } from '@angular/forms';
import { CreateOrgModalComponent } from './create-org-modal.component';
import { MockComponent } from 'ng2-mock-component';
import { MockAppProjectsDropdown, MockAppResourceDropdown, MockChefButton, MockChefCheckbox, MockChefError, MockChefFormField, MockChefLoadingSpinner, MockChefModal, MockChefToolbar } from 'app/testing/mock-components';
import { RouterTestingModule } from '@angular/router/testing';
import { CommonModule } from '@angular/common';
import { BrowserModule } from '@angular/platform-browser';

describe('CreateOrgModalComponent', () => {
  let component: CreateOrgModalComponent;
  let fixture: ComponentFixture<CreateOrgModalComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        CreateOrgModalComponent
      ],
      imports: [
        CommonModule,
        BrowserModule,
        ReactiveFormsModule,
        RouterTestingModule,
        MockChefButton,
        MockChefLoadingSpinner,
        MockChefFormField,
        MockChefCheckbox,
        MockChefError,
        MockChefToolbar,
        MockChefModal,
        MockAppResourceDropdown,
        MockAppProjectsDropdown
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(CreateOrgModalComponent);
    component = fixture.componentInstance;
    component.createForm = new FormBuilder().group({
      name: ['', null],
      admin_user: ['', null],
      admin_key: ['', null]
    });
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
