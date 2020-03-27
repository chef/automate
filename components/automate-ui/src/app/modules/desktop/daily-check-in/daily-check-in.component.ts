import { Component, Input } from '@angular/core';

@Component({
  selector: 'app-daily-check-in',
  templateUrl: './daily-check-in.component.html',
  styleUrls: ['./daily-check-in.component.scss']
})
export class DailyCheckInComponent {

  @Input() unknownPercentage: number;
  @Input() checkedInPercentage: number;
  @Input() totalCount: number;
  @Input() unknownCount: number;
  @Input() checkedInCount: number;

  constructor() { }
}
