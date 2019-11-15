import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { MockComponent } from 'ng2-mock-component';
import { NO_ERRORS_SCHEMA } from '@angular/core';
import { StoreModule, Store } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, defaultInitialState, runtimeChecks } from 'app/ngrx.reducers';

import { ChefNotificationsComponent } from './notifications.component';

describe('ChefNotificationsComponent', () => {
  let component: ChefNotificationsComponent;
  let fixture: ComponentFixture<ChefNotificationsComponent>;
  let store: Store<NgrxStateAtom>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'chef-notification', inputs: ['type', 'timeout'] }),
        ChefNotificationsComponent
      ],
      imports: [
        StoreModule.forRoot(ngrxReducers, { ...defaultInitialState, runtimeChecks })
      ],
      schemas: [
        NO_ERRORS_SCHEMA
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    store = TestBed.get(Store);
    spyOn(store, 'dispatch').and.callThrough();
    fixture = TestBed.createComponent(ChefNotificationsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
