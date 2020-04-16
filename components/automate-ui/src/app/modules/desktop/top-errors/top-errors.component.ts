import { Component, Input, Output, OnInit, OnDestroy, OnChanges,
  SimpleChange, EventEmitter } from '@angular/core';
import { Subject, timer } from 'rxjs';
import { takeUntil } from 'rxjs/operators';
import { maxBy } from 'lodash/fp';
import { TimeFromNowPipe } from 'app/pipes/time-from-now.pipe';

import {
  TopErrorsItem
} from 'app/entities/desktop/desktop.model';

@Component({
  selector: 'app-top-errors',
  templateUrl: './top-errors.component.html',
  styleUrls: ['./top-errors.component.scss']
})
export class TopErrorsComponent  implements OnInit, OnDestroy, OnChanges  {

  @Input() topErrorsItems: TopErrorsItem[];
  @Input() lastUpdated: Date;
  @Output() errorSelected: EventEmitter<TopErrorsItem> = new EventEmitter();

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

  get topErrorsItemsMax() {
    return maxBy('count', this.topErrorsItems);
  }

  public onItemClicked(item: TopErrorsItem) {
    this.errorSelected.emit(item);
  }
}
