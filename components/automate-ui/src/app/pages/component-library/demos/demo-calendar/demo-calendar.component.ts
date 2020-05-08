import { Component } from '@angular/core';
import * as moment from 'moment/moment';

@Component({
  selector: 'app-demo-calendar',
  templateUrl: './demo-calendar.component.html',
  styleUrls: ['./demo-calendar.component.scss']
})
export class DemoCalendarComponent {

  date = moment();

  handleMonthClick([month, year]) {
    this.date.month(month);
    this.date.year(year);
  }

  handleDaySelect(date) {
    this.date = moment(date);
  }
}
