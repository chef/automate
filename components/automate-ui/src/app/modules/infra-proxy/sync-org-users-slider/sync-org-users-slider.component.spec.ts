import { ComponentFixture, TestBed } from '@angular/core/testing';

import { SyncOrgUsersSliderComponent } from './sync-org-users-slider.component';

describe('SyncOrgUsersSliderComponent', () => {
  let component: SyncOrgUsersSliderComponent;
  let fixture: ComponentFixture<SyncOrgUsersSliderComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ SyncOrgUsersSliderComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(SyncOrgUsersSliderComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
