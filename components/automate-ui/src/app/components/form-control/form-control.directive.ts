import { Directive, OnDestroy, OnInit, Input, OnChanges, SimpleChanges } from '@angular/core';
import { NgControl } from '@angular/forms';
import { Subject } from 'rxjs';
import { distinctUntilChanged, takeUntil } from 'rxjs/operators';

@Directive({
  selector: '[formControl],[formControlName]'
})
export class FormControlDirective implements OnInit, OnDestroy, OnChanges {
  @Input() resetOrigin = false;

  constructor(private control: NgControl) {}

  private originalValue: any;
  private isDestroyed$ = new Subject<boolean>();

  ngOnInit(): void {
    this.setOriginalValue();

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

  ngOnDestroy(): void {
    this.isDestroyed$.next(true);
    this.isDestroyed$.complete();
  }

  ngOnChanges(changes: SimpleChanges): void {
    if (changes.resetOrigin) {
      const resetRequested: boolean = changes.resetOrigin.currentValue;
      if (resetRequested) {
        this.setOriginalValue();
      }
    }
  }

  private setOriginalValue(): void {
    this.originalValue = this.control.value;
  }
}
