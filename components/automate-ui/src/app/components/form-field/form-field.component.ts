import { Component,
         ContentChildren,
         QueryList,
         AfterContentInit,
         ChangeDetectionStrategy,
         ChangeDetectorRef
       } from '@angular/core';
import { ErrorDirective } from '../error/error.directive';

@Component({
  selector: 'chef-form-field',
  templateUrl: './form-field.component.html',
  styleUrls: ['./form-field.component.scss',
              '../error/error.directive.scss',
              '../input/input.directive.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush
})
export class FormFieldComponent implements AfterContentInit {
  @ContentChildren(ErrorDirective) errorChildren: QueryList<ErrorDirective>;

  constructor(public changeDetectorRef: ChangeDetectorRef) {}

  ngAfterContentInit() {
    this.errorChildren.changes.subscribe(() => {
      // This component uses ChangeDetectionStrategy.OnPush so we have to tell
      // it to run change detection if it's children change.
      this.changeDetectorRef.markForCheck();
    });
  }

}
