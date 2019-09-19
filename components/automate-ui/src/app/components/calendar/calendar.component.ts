import { Component,
         Input,
         Output,
         EventEmitter,
         ChangeDetectionStrategy } from '@angular/core';
import * as m from 'moment';
import { concat,
         range,
         rangeRight,
         map } from 'lodash';

@Component({
  selector: 'chef-calendar',
  templateUrl: './calendar.component.html',
  styleUrls: ['./calendar.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush
})
export class CalendarComponent {

  private _date: m.Moment = m();
  private _selected: m.Moment = m();

  private _month: string;
  private _year: number;

  @Output() onNextMonth: EventEmitter<[number, number]> = new EventEmitter();
  @Output() onPrevMonth: EventEmitter<[number, number]> = new EventEmitter();
  @Output() onDaySelect: EventEmitter<string> = new EventEmitter();

  constructor() {
    this.month = this.date.month().toString();
    this.year = this.date.year();
  }

  @Input()
  set date(input) {
    const date = m.isMoment(input) ? input : m(input);
    this._date = date;
    this._month = m.months(date.month());
    this._year = date.year();
  }
  get date() {
    return this._date;
  }

  @Input()
  set selected(input) {
    const date = m.isMoment(input) ? input : m(input);
    this._selected = date;
  }
  get selected() {
    return this._selected;
  }

  @Input()
  set month(input: string) {
    // Set the month on the internal date.
    this.date.month(input);
    // Set the normalized internal month to its string
    // representation for display.
    this._month = m.months(this.date.month());
  }
  get month(): string {
    return this._month;
  }

  @Input()
  set year(input: number) {
    this._year = parseInt(input.toString(), 10);
    this.date.year(this._year);
  }
  get year(): number {
    return this._year;
  }

  // returns the days to display on the calendar
  get days() {
    const vm = this.date.month();
    const vy = this.date.year();
    const sm = this.selected.month();
    const sy = this.selected.year();
    const today = m().month() === this.date.month() ? m().date() : null;
    const selected = (vm === sm && vy === sy) ? this.selected.date() : null;
    const tag = (marker) => (d: m.Moment) => [marker, d];
    const tagActive = (d: m.Moment) => {
      switch (d.date()) {
      case selected:
        return tag('a s')(d);
      case today:
        return tag('a t')(d);
      default:
        return tag('a')(d);
      }
    };

    return concat(map(this.daysBefore(), tag('i')),
                  map(this.daysDuring(), tagActive),
                  map(this.daysAfter(), tag('i')));
  }

  // returns the names of the weekdays in a minimal form:
  // [Mo, Tu, We, Th, Fr, Sa, Su]
  get weekdays(): string[] {
    return m.weekdaysMin();
  }

  // When the first of the month is on any day other than the first
  // day of the week, we need to fill in those extra days. This
  // method returns the last few days of the previous month.
  private daysBefore(): m.Moment[] {
    const lastMonth = this
      .date
      .clone()
      .subtract(1, 'month');

    const daysInLastMonth = lastMonth.daysInMonth();
    const firstDay = this
      .date
      .clone()
      .date(1)
      .day();

    return rangeRight(daysInLastMonth, daysInLastMonth - firstDay).map(
      (day: number) => lastMonth.clone().date(day));
  }

  // Returns the range of dates during the month
  private daysDuring(): m.Moment[] {
    const daysInThisMonth = this
      .date
      .daysInMonth();

    return range(1, daysInThisMonth + 1).map((day: number) => this.date.clone().date(day));
  }

  // Returns the first few days of the next month to fill out
  // the rest of the week.
  private daysAfter(): m.Moment[] {
    const nextMonth = this
      .date
      .clone()
      .add(1, 'month');

    return range(1, 8 - ((this.daysBefore().length + this.daysDuring().length) % 7 || 7)).map(
      (day: number) => nextMonth.clone().date(day));
  }

  public onNextMonthClick() {
    const nextMonth = this.date.clone().add(1, 'month');
    this.onNextMonth.emit([nextMonth.month(), nextMonth.year()]);
  }

  public onPrevMonthClick() {
    const prevMonth = this.date.clone().subtract(1, 'month');
    this.onPrevMonth.emit([prevMonth.month(), prevMonth.year()]);
  }

  public onDayClick(day) {
    this.onDaySelect.emit(day[1].format('YYYY-MM-DD'));
  }

  trackBy(index, _item) {
    return index;
  }

}
