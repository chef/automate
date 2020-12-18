import { Injectable, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { Observable, BehaviorSubject } from 'rxjs';
import { StoreModule, Store } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, defaultInitialState, runtimeChecks } from 'app/ngrx.reducers';

import { LayoutFacadeService } from 'app/entities/layout/layout.facade';
import { LicenseNotificationsComponent } from './license-notifications.component';
import { LicenseFacadeService } from 'app/entities/license/license.facade';
import { Notification } from 'app/entities/notifications/notification.model';

describe('LicenseNotificationsComponent', () => {
  let store: Store<NgrxStateAtom>;
  let component: LicenseNotificationsComponent;
  let fixture: ComponentFixture<LicenseNotificationsComponent>;

  @Injectable()
  class MockLicenseFacadeService extends LicenseFacadeService {
    notifications$: Observable<Notification[]> = new BehaviorSubject([]);
  }

  @Injectable()
  class MockLayoutFacadeService extends LicenseFacadeService {
    layout = {
      license: {
        display: true
      }
    };
  }

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      imports: [
        StoreModule.forRoot(ngrxReducers, { initialState: defaultInitialState, runtimeChecks })
      ],
      declarations: [
        LicenseNotificationsComponent
      ],
      providers: [
        { provide: LicenseFacadeService, useClass:  MockLicenseFacadeService },
        { provide: LayoutFacadeService, useClass:  MockLayoutFacadeService }
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    }).compileComponents();
      store = TestBed.inject(Store);
      spyOn(store, 'dispatch').and.callThrough();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(LicenseNotificationsComponent);
    component = fixture.componentInstance;
    store = TestBed.inject(Store);
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
