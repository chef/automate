import { Component, Input, OnInit, OnDestroy, OnChanges, Output, EventEmitter, SimpleChange } from '@angular/core';
import { Subject, timer } from 'rxjs';
import { takeUntil } from 'rxjs/operators';
import { maxBy } from 'lodash/fp';
import { TimeFromNowPipe } from 'app/pipes/time-from-now.pipe';

import {
  CountedDurationItem
} from 'app/entities/desktop/desktop.model';

@Component({
  selector: 'app-unknown-desktop-duration-counts',
  templateUrl: './unknown-desktop-duration-counts.component.html',
  styleUrls: ['./unknown-desktop-duration-counts.component.scss']
})
export class UnknownDesktopDurationCountsComponent  implements OnInit, OnDestroy, OnChanges  {

  @Input() countedDurationItems: CountedDurationItem[];
  @Input() lastUpdated: Date;
  @Input() selectedDuration: string;

  @Output() durationSelected: EventEmitter<string> = new EventEmitter();

  private isDestroyed = new Subject<boolean>();
  public lastUpdatedMessage = '-';
  private timeFromNowPipe = new TimeFromNowPipe();
  private minute = 6000;

  constructor() { }

  ngOnInit(): void {
    timer(1, this.minute).pipe(
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

  get countedDurationItemsMax() {
    return maxBy('count', this.countedDurationItems);
  }

  onDurationSelect(duration: string) {
    this.durationSelected.emit(duration !== this.selectedDuration ? duration : undefined);
  }

  // Make this more general once we don't have hard coded durations.
  formatDuration(duration: string): string {
    switch (duration) {
      case '1M':
        return 'Over a month';
      case '2w':
        return '2 Weeks';
      case '1w':
        return '1 Week';
      case '3d':
        return '3 Days';

      default:
        return duration;
    }
  }

  get hasData(): boolean {
    return this.countedDurationItems.some(item => item.count > 0);
  }
}
