import { EventEmitter } from '@angular/core';
import { ReactiveFormsModule, FormBuilder } from '@angular/forms';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { MockComponent } from 'ng2-mock-component';

import { CreateV1TeamModalComponent } from './create-v1-team-modal.component';

describe('CreateV1TeamModalComponent', () => {
  let component: CreateV1TeamModalComponent;
  let fixture: ComponentFixture<CreateV1TeamModalComponent>;

  beforeEach(async () => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-toolbar' }),
        MockComponent({ selector: 'chef-modal',
          inputs: ['visible'],
          outputs: ['close']
        }),
        CreateV1TeamModalComponent
      ],
      imports: [
        ReactiveFormsModule
      ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(CreateV1TeamModalComponent);
    component = fixture.componentInstance;
    component.createForm = new FormBuilder().group({
      name: ['', null],
      description: ['', null]
    });
    component.conflictErrorEvent = new EventEmitter();
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
