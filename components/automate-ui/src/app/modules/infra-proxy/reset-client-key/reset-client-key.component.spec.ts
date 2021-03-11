import { ComponentFixture, TestBed } from '@angular/core/testing';
import { MockComponent } from 'ng2-mock-component';
import { EventEmitter } from '@angular/core';
import { ResetClientKeyComponent } from './reset-client-key.component';
import { StoreModule } from '@ngrx/store';
import { ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';

describe('ResetClientKeyComponent', () => {
  let component: ResetClientKeyComponent;
  let fixture: ComponentFixture<ResetClientKeyComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'chef-modal', inputs: ['visible'] }),
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-loading-spinner'}),
        ResetClientKeyComponent ],
        imports: [
          StoreModule.forRoot(ngrxReducers, { runtimeChecks })
        ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(ResetClientKeyComponent);
    component = fixture.componentInstance;
    component.openEvent = new EventEmitter();
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
