import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { MockComponent } from 'ng2-mock-component';
import { NO_ERRORS_SCHEMA } from '@angular/core';
import { StoreModule } from '@ngrx/store';
import { runtimeChecks } from 'app/ngrx.reducers';

import { ChefNotificationsComponent } from './notifications.component';
import { notificationEntityReducer } from 'app/entities/notifications/notification.reducer';

describe('ChefNotificationsComponent', () => {
  let component: ChefNotificationsComponent;
  let fixture: ComponentFixture<ChefNotificationsComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'chef-notification', inputs: ['type', 'timeout'] }),
        ChefNotificationsComponent
      ],
      imports: [
        StoreModule.forRoot({
          notifications: notificationEntityReducer
        }, { runtimeChecks })
      ],
      schemas: [
        NO_ERRORS_SCHEMA
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ChefNotificationsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
