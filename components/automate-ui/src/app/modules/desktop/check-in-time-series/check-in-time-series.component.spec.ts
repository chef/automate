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
});
