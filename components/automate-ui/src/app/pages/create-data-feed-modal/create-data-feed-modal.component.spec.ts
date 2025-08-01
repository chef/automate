import { EventEmitter } from '@angular/core';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { ReactiveFormsModule, FormBuilder } from '@angular/forms';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { MockComponent } from 'ng2-mock-component';
import { MockChefButton, MockChefError, MockChefFormField, MockChefLoadingSpinner, MockChefModal, MockChefToolbar } from 'app/testing/mock-components';
import { CreateDataFeedModalComponent } from './create-data-feed-modal.component';

describe('CreateChefServerModalComponent', () => {
  let component: CreateDataFeedModalComponent;
  let fixture: ComponentFixture<CreateDataFeedModalComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        CreateDataFeedModalComponent
      ],
      imports: [
        ReactiveFormsModule,
        MockChefButton,
        MockChefLoadingSpinner,
        MockChefFormField,
        MockChefError,
        MockChefToolbar,
        MockChefModal
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(CreateDataFeedModalComponent);
    component = fixture.componentInstance;
    component.createForm = new FormBuilder().group({
      id: ['', null],
      name: ['', null],
      url: ['', null],
      username: ['', null],
      password: ['', null]
    });
    component.conflictErrorEvent = new EventEmitter();
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
