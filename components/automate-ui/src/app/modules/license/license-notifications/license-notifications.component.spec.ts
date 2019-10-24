import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { MockComponent } from 'ng2-mock-component';

import { LicenseNotificationsComponent } from './license-notifications.component';

describe('NotificationsComponent', () => {
  let component: LicenseNotificationsComponent;
  let fixture: ComponentFixture<LicenseNotificationsComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'chef-notification', inputs: ['type', 'timeout'] }),
        LicenseNotificationsComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(LicenseNotificationsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
