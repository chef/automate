import { Component, EventEmitter, Input, OnInit, OnDestroy, OnChanges, Output, SimpleChange } from '@angular/core';
import { Subject, timer } from 'rxjs';
import { takeUntil } from 'rxjs/operators';
import { TimeFromNowPipe } from 'app/pipes/time-from-now.pipe';

@Component({
  selector: 'app-daily-check-in',
  templateUrl: './daily-check-in.component.html',
  styleUrls: ['./daily-check-in.component.scss']
})
export class DailyCheckInComponent implements OnInit, OnDestroy, OnChanges {

  @Input() unknownPercentage: number;
  @Input() checkedInPercentage: number;
  @Input() totalCount: number;
  @Input() unknownCount: number;
  @Input() checkedInCount: number;
  @Input() lastUpdated: Date;
  @Input() selectedStatus: string;

  @Output() statusSelected: EventEmitter<string> = new EventEmitter<string>();

  private isDestroyed = new Subject<boolean>();
  public lastUpdatedMessage = '-';
  private timeFromNowPipe = new TimeFromNowPipe();
  private minute = 6000;

  constructor() { }

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
    return this.totalCount > 0;
  }
}
