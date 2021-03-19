import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { StoreModule } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';
import { ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { DeleteInfraObjectModalComponent } from './delete-infra-object-modal.component';
import { EventEmitter } from '@angular/core';
import { HttpClient, HttpHandler } from '@angular/common/http';
import { MatSelectModule } from '@angular/material/select';

describe('DeleteInfraObjectModalComponent', () => {
  let component: DeleteInfraObjectModalComponent;
  let fixture: ComponentFixture<DeleteInfraObjectModalComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'chef-modal', inputs: ['visible'] }),
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-error' }),
        DeleteInfraObjectModalComponent
      ],
      imports: [
        MatSelectModule,
        ReactiveFormsModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ],
      providers: [
        HttpClient,
        HttpHandler
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(DeleteInfraObjectModalComponent);
    component = fixture.componentInstance;
    component.close = new EventEmitter();
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
