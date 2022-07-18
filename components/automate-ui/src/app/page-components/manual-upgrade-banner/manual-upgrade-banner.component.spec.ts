import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ManualUpgradeBannerComponent } from './manual-upgrade-banner.component';

describe('ManualUpgradeBannerComponent', () => {
  let component: ManualUpgradeBannerComponent;
  let fixture: ComponentFixture<ManualUpgradeBannerComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ ManualUpgradeBannerComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(ManualUpgradeBannerComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
