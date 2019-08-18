import { Directive, OnDestroy, OnInit, Input, OnChanges, SimpleChanges } from '@angular/core';
import { NgControl } from '@angular/forms';
import { Subject } from 'rxjs';
import { distinctUntilChanged, takeUntil } from 'rxjs/operators';

// Getting the most from an Angular form
// *************************************
// Out of the box, this directive automatically provides "memory" to your form
// controls (typically <chef-input>, <chef-radio>, and <chef-select> elements)
// when a focussed control is being edited.
// Should the user change the value back to its original value,
// the control is reset to a pristine state.
// You can make use of that fact by, e.g., disabling your Save button,
// since the field has no new value to be saved:
//      <chef-button [disabled]="!yourForm.dirty || ..."
//
// If, upon a successful operation, the user is taken elsewhere
// (e.g. the modal closes, or the page navigates to a summary, etc.)
// nothing further is required.
// If, however, the form (or modal) remains open, you typically want
// to have the "pristine state" of the control be updated
// to the newly processed value. Here's how to make that happen:
//
// 1. Add the `resetOrigin` property to each control in your form,
//    assigning them all the same flag value, in this case `saveSuccessful`:
//    <input formControlName="name" type="text" [resetOrigin]="saveSuccessful" />
//
// 2. In your function that processes the form, set that flag to false
//    and then change it to true once your operation is confirmed successful.
//    That is all you need to complete the "memory" of the form control.
//
// 3. Hearkening back to providing the `[disabled]` functionality on the Save button
//    above, though, you need one more thing.
//    Once you have confirmed the operation successful,
//    you also need to mark the form pristine so that `dirty` check will
//    initialize your Save button to disabled, which is typically desired
//    immediately following the Save.
//
// Implementing (2) and (3) would look something like this:
//
//  saveStuff(): void {
//    this.saveSuccessful = false;
//    // ...perform the save and get a status back...
//    this.saveSuccessful = (state === EntityStatus.loadingSuccess);
//    if (this.saveSuccessful) {
//      this.myForm.markAsPristine();
//    }

@Directive({
  selector: '[formControl],[formControlName]'
})
export class FormControlDirective implements OnInit, OnDestroy, OnChanges {

  // Whenever you set this to true it resets the origin to the current value of the control.
  // Typically it should be initialized to false.
  // When you process a form (e.g. doing a "save" operation) start by setting this to false.
  // When the save is complete--and successful--then set it to true,
  // signalling that the new, present value should now be considered the base for comparison.
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
