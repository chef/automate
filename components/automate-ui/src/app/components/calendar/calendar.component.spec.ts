import { Component, DebugElement, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';

import { CalendarComponent } from './calendar.component';

@Component({
  template: `
<chef-calendar class="cal1"
               [month]="month"
               [year]="year"
               [selected]="selected">
</chef-calendar>

<chef-calendar class="cal2"
               [date]="date">
</chef-calendar>
`
})
class TestHostComponent {
  month: string | number = 'January';
  year = 2017;
  selected = '2017-01-01';
  date = '2017-01-01';
}

describe('CalendarComponent', () => {
  let component: TestHostComponent;
  let fixture: ComponentFixture<TestHostComponent>;
  let calendarEl: DebugElement;     // Calendar configured with separate attributes
  let calendarDateEl: DebugElement; // Calendar configured with date attribute

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [ TestHostComponent, CalendarComponent ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TestHostComponent);
    component = fixture.componentInstance;
    calendarEl = fixture.debugElement.query(By.css('.cal1'));
    calendarDateEl = fixture.debugElement.query(By.css('.cal2'));
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });

  describe('header', () => {
    let headerEl;
    let headerDateEl;

    beforeEach(() => {
      headerEl = calendarEl.query(By.css('.header'));
      headerDateEl = calendarDateEl.query(By.css('.header'));
    });

    it('displays the correct month when set via it\'s full name', () => {
      component.month = 'March';
      fixture.detectChanges();

      expect(headerEl.nativeElement.textContent).toContain('March');
    });

    it('displays the correct month when set via it\'s ordinal', () => {
      component.month = 3;
      fixture.detectChanges();

      expect(headerEl.nativeElement.textContent).toContain('April');
    });

    it('displays the correct month when set via it\s abbreviation', () => {
      component.month = 'jun';
      fixture.detectChanges();

      expect(headerEl.nativeElement.textContent).toContain('June');
    });

    it('displays the correct month when set via the date input', () => {
      component.date = '2017-09-01';
      fixture.detectChanges();

      expect(headerDateEl.nativeElement.textContent).toContain('September');
    });

    it('displays the correct year', () => {
      component.year = 2018;
      fixture.detectChanges();

      expect(headerEl.nativeElement.textContent).toContain('2018');
    });

    it('displays the correct year when set via the date input', () => {
      component.date = '2012-07-01';
      fixture.detectChanges();

      expect(headerDateEl.nativeElement.textContent).toContain('2012');
    });
  });

  describe('calendar', () => {

    it('displays the correct number of active days', () => {
      component.date = '2017-11-01';
      fixture.detectChanges();

      const activeDays = calendarDateEl.queryAll(By.css('.day.a'));

      expect(activeDays.length).toBe(30);
    });

    it('displays the correct number of inactive days', () => {
      component.date = '2017-02-01';
      fixture.detectChanges();

      const inactiveDays = calendarDateEl.queryAll(By.css('.day.i'));

      expect(inactiveDays.length).toBe(7);
    });

    it('selects the correct day', () => {
      component.selected = '2017-01-20';
      fixture.detectChanges();

      const selectedDay = calendarEl.query(By.css('.day.s'));

      expect(selectedDay.nativeElement.textContent).toContain('20');
    });
  });
});
