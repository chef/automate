import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { CreateOrgModalComponent } from './create-org-modal.component';

describe('CreateOrgModalComponent', () => {
  let component: CreateOrgModalComponent;
  let fixture: ComponentFixture<CreateOrgModalComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ CreateOrgModalComponent ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(CreateOrgModalComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
