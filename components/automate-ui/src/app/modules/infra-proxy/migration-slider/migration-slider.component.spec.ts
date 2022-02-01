import { ComponentFixture, TestBed } from '@angular/core/testing';

import { MigrationSliderComponent } from './migration-slider.component';

describe('MigrationSliderComponent', () => {
  let component: MigrationSliderComponent;
  let fixture: ComponentFixture<MigrationSliderComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ MigrationSliderComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(MigrationSliderComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
