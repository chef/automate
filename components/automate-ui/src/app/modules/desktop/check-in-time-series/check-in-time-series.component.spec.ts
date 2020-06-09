import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { CheckInTimeSeriesComponent } from './check-in-time-series.component';

describe('CheckInTimeSeriesComponent', () => {
  let fixture: ComponentFixture<CheckInTimeSeriesComponent>;
  let component: CheckInTimeSeriesComponent;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [],
      declarations: [
        CheckInTimeSeriesComponent
      ],
      providers: [],
      schemas: [CUSTOM_ELEMENTS_SCHEMA]
    });

    fixture = TestBed.createComponent(CheckInTimeSeriesComponent);
    component = fixture.componentInstance;
  });

  it('should exist', () => {
    expect(component).toBeTruthy();
  });
});
