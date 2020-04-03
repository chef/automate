import { Component, Input, OnInit, OnDestroy, OnChanges, SimpleChange } from '@angular/core';
import { Subject, timer } from 'rxjs';
import { takeUntil } from 'rxjs/operators';
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
}
