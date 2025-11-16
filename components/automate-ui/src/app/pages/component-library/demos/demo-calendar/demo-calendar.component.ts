import { Component } from '@angular/core';
import moment from 'moment';

@Component({
  standalone: false,
  selector: 'app-demo-calendar',
  templateUrl: './demo-calendar.component.html',
  styleUrls: ['./demo-calendar.component.scss']
})
export class DemoCalendarComponent {

  date = moment();

  handleMonthClick(e: any) {
    const [month, year] = e;
    this.date.month(month);
    this.date.year(year);
  }

  handleDaySelect(date) {
    this.date = moment(date);
  }
}
