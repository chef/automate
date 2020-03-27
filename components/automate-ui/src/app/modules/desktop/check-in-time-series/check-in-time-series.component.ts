import { Component, Input, Output, EventEmitter } from '@angular/core';

import {
  DayPercentage
} from 'app/entities/desktop/desktop.model';

@Component({
  selector: 'app-check-in-time-series',
  templateUrl: './check-in-time-series.component.html',
  styleUrls: ['./check-in-time-series.component.scss']
})
export class CheckInTimeSeriesComponent {
  @Input() days: DayPercentage[];
  @Input() selectedDaysAgo: number;
  @Output() daysAgoChanged: EventEmitter<number> = new EventEmitter();
  public selectableDaysAgoCollection = [3, 7, 14];

  constructor() {}

  dateChanged(selectedDaysAgo: number) {
    this.daysAgoChanged.emit(selectedDaysAgo);
  }
}
