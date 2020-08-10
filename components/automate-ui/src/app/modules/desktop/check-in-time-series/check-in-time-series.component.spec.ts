import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { CheckInTimeSeriesComponent } from './check-in-time-series.component';
import { By } from '@angular/platform-browser';

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

  describe('information tooltip', () => {
    it('should have an element on page with matching id', () => {
      const debugElement = fixture.debugElement.query(By.css('#checkin-history-info'));
      expect(debugElement).toBeTruthy();
    });

    it('should contain "for" attribute linked to matching element', () => {
      const tooltip = fixture.debugElement.query(By.css('chef-tooltip')).nativeElement;
      expect(tooltip.getAttribute('for')).toEqual('checkin-history-info');
    });
  });

  describe('when data is available', () => {
    it('does not display empty data message', () => {
      component.days = [
        { daysAgo: 0, percentage: 60, total: 10 },
        { daysAgo: 1, percentage: 40, total: 10 }
      ];

      fixture.detectChanges();

      const emptyMsg = fixture.debugElement.query(By.css('app-empty-data'));
      expect(emptyMsg).toBeFalsy();
    });
  });

  describe('when no data is available', () => {
    it('displays empty data message', () => {
      component.days = [
        { daysAgo: 0, percentage: 100, total: 0 },
        { daysAgo: 1, percentage: 100, total: 0 }
      ];

      fixture.detectChanges();

      const emptyMsg = fixture.debugElement.query(By.css('app-empty-data'));
      expect(emptyMsg).toBeTruthy();
    });
  });
});
