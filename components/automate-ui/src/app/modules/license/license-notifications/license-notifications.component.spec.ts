import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { LicenseNotificationsComponent } from './license-notifications.component';

describe('NotificationsComponent', () => {
  let component: LicenseNotificationsComponent;
  let fixture: ComponentFixture<LicenseNotificationsComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ LicenseNotificationsComponent ]
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
