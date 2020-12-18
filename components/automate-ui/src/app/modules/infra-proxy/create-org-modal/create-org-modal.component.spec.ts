import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { ReactiveFormsModule, FormBuilder } from '@angular/forms';
import { CreateOrgModalComponent } from './create-org-modal.component';
import { MockComponent } from 'ng2-mock-component';
import { RouterTestingModule } from '@angular/router/testing';
import { CommonModule } from '@angular/common';
import { BrowserModule } from '@angular/platform-browser';

describe('CreateOrgModalComponent', () => {
  let component: CreateOrgModalComponent;
  let fixture: ComponentFixture<CreateOrgModalComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        CreateOrgModalComponent,
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-checkbox' }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-toolbar' }),
        MockComponent({ selector: 'chef-modal', inputs: ['visible']}),
        MockComponent({ selector: 'app-resource-dropdown',
        inputs: ['resources', 'resourcesUpdated', 'objectNounPlural'] }),
        MockComponent({ selector: 'app-projects-dropdown',
        inputs: ['projects', 'projectsUpdated'] })
      ],
      imports: [
        CommonModule,
        BrowserModule,
        ReactiveFormsModule,
        RouterTestingModule
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
