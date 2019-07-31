import { Component,
         ContentChildren,
         QueryList,
         AfterContentInit,
         ChangeDetectionStrategy,
         ChangeDetectorRef,
         Input,
         OnChanges,
         SimpleChanges,
         SimpleChange
       } from '@angular/core';
import { FormControl } from '@angular/forms';
import { ErrorDirective } from '../error/error.directive';

@Component({
  selector: 'chef-form-field',
  templateUrl: './form-field.component.html',
  styleUrls: ['./form-field.component.scss',
              '../error/error.directive.scss',
              '../input/input.directive.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush
})
export class FormFieldComponent implements AfterContentInit, OnChanges {
  @ContentChildren(ErrorDirective) errorChildren: QueryList<ErrorDirective>;

  // A FormFieldComponent can optionally track whether an input field (FormControl)
  // being edited is the same as, or different from, the field's initial value.
  // That is, it knows whether the user has made a change or not,
  // and marks the pristine/dirty state accordingly, so that information may be used
  // in the parent form to e.g., disable the `Save` button, per our UX guidelines.
  // Example:
  // <form [formGroup]="updateForm">
  //   <chef-form-field
  //       [watch]="updateForm.controls.name.value"
  //       [control]="updateForm.controls.name">
  //     <label>Name <span aria-hidden="true">*</span></label>
  //     <chef-input formControlName="name" type="text"></chef-input>
  //   </chef-form-field>
  // </form>
  // <chef-button primary [disabled]="!updateForm.valid || !updateForm.dirty">
  //
  // You need to supply these two parameters:
  @Input() watch: string; //  dynamic value to watch
  @Input() control: FormControl; // control to set back to pristine when needed

  private originalValue = '';

  constructor(public changeDetectorRef: ChangeDetectorRef) {}

  ngAfterContentInit() {
    this.errorChildren.changes.subscribe(() => {
      // This component uses ChangeDetectionStrategy.OnPush so we have to tell
      // it to run change detection if it's children change.
      this.changeDetectorRef.markForCheck();
    });
  }

  ngOnChanges(changes: SimpleChanges): void {
    const change: SimpleChange = changes.watch;
    if (!change.previousValue && change.currentValue && !this.originalValue) {
      this.originalValue = change.currentValue;
    }
    if (change.currentValue && (change.currentValue === this.originalValue)) {
      this.control.markAsPristine();
    }
  }
}
