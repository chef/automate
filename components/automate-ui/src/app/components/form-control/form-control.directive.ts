import { Directive, OnDestroy, OnInit } from '@angular/core';
import { NgControl } from '@angular/forms';
import { Subject } from 'rxjs';
import { distinctUntilChanged, takeUntil } from 'rxjs/operators';

@Directive({
  selector: '[formControl],[formControlName]'
})
export class FormControlDirective implements OnInit, OnDestroy {

  constructor(private control: NgControl) {}

  private originalValue: any;

  private isDestroyed$: Subject<boolean> = new Subject<boolean>();

  ngOnInit() {
    this.originalValue = this.control.value;

    this.control.valueChanges
      .pipe(
        takeUntil(this.isDestroyed$),
        distinctUntilChanged()
      )
      .subscribe(newValue => {
        if (newValue === this.originalValue) {
          this.control.reset(this.originalValue);
        }
      });
  }

  ngOnDestroy() {
    this.isDestroyed$.next(true);
    this.isDestroyed$.complete();
  }
}
