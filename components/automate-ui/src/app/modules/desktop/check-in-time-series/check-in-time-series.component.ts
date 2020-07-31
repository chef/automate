import { Component, Input, Output, EventEmitter,
  OnInit, OnDestroy, OnChanges, SimpleChange } from '@angular/core';
import { Subject, timer } from 'rxjs';
import { takeUntil } from 'rxjs/operators';

import { TimeFromNowPipe } from 'app/pipes/time-from-now.pipe';
import {
  DayPercentage
} from 'app/entities/desktop/desktop.model';

@Component({
  selector: 'app-check-in-time-series',
  templateUrl: './check-in-time-series.component.html',
  styleUrls: ['./check-in-time-series.component.scss']
})
export class CheckInTimeSeriesComponent implements OnInit, OnDestroy, OnChanges  {
  @Input() days: DayPercentage[];
  @Input() selectedDaysAgo: number;
  @Input() lastUpdated: Date;
  @Output() daysAgoChanged: EventEmitter<number> = new EventEmitter();
  public selectableDaysAgoCollection = [3, 7, 14];
  private isDestroyed = new Subject<boolean>();
  public lastUpdatedMessage = '-';
  private timeFromNowPipe = new TimeFromNowPipe();
  private minute = 6000;

  constructor() {}

  dateChanged(selectedDaysAgo: number) {
    this.daysAgoChanged.emit(selectedDaysAgo);
  }

  ngOnInit(): void {
    timer(0, this.minute).pipe(
      takeUntil(this.isDestroyed)
    ).subscribe(() => {
      this.lastUpdatedMessage = this.timeFromNowPipe.transform(this.lastUpdated);
    });
  }

  ngOnChanges(changes: {[propertyName: string]: SimpleChange}) {
    if (changes['lastUpdated']) {
      this.lastUpdatedMessage = this.timeFromNowPipe.transform(changes['lastUpdated'].currentValue);
    }
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  get hasData(): boolean {
    return this.days && this.days.some(item => item.total > 0);
  }
}
