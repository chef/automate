import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { DailyCheckInComponent } from './daily-check-in.component';
import { By } from '@angular/platform-browser';

describe('DailyCheckInComponent', () => {
  let fixture: ComponentFixture<DailyCheckInComponent>;
  let component: DailyCheckInComponent;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [],
      declarations: [
        DailyCheckInComponent
      ],
      providers: [],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });

    fixture = TestBed.createComponent(DailyCheckInComponent);
    component = fixture.componentInstance;
  });

  it('should exist', () => {
    expect(component).toBeTruthy();
  });

  describe('information tooltip', () => {
    it('should have an element on page with matching id', () => {
      const debugElement = fixture.debugElement.query(By.css('#daily-checkin-info'));
      expect(debugElement).toBeTruthy();
    });

    it('should contain "for" attribute linked to matching element', () => {
      const tooltip = fixture.debugElement.query(By.css('chef-tooltip')).nativeElement;
      expect(tooltip.getAttribute('for')).toEqual('daily-checkin-info');
    });
  });

  describe('when data is available', () => {
    it('does not display empty data message', () => {
      component.totalCount = 10;

      fixture.detectChanges();

      const emptyMsg = fixture.debugElement.query(By.css('app-empty-data'));
      expect(emptyMsg).toBeFalsy();
    });
  });

  describe('when no data is available', () => {
    it('displays empty data message', () => {
      component.totalCount = 0;

      fixture.detectChanges();

      const emptyMsg = fixture.debugElement.query(By.css('app-empty-data'));
      expect(emptyMsg).toBeTruthy();
    });
  });
});
