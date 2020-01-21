import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { StoreModule } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';

import { ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { CreateUserModalComponent } from './create-user-modal.component';
import { EventEmitter } from '@angular/core';

describe('CreateUserModalComponent', () => {
  let component: CreateUserModalComponent;
  let fixture: ComponentFixture<CreateUserModalComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'chef-modal', inputs: ['visible'] }),
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-toolbar' }),
        CreateUserModalComponent
      ],
      imports: [
        ReactiveFormsModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ]
    })
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(CreateUserModalComponent);
    component = fixture.componentInstance;
    component.openEvent = new EventEmitter();
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
