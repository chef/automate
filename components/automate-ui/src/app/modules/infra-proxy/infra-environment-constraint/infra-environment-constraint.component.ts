import {
  Component,
  OnInit,
  Input,
  Output,
  EventEmitter
} from '@angular/core';

import { FormBuilder, Validators, FormGroup } from '@angular/forms';
import { Regex } from 'app/helpers/auth/regex';

import { CookbookConstraintGrid } from '../create-environment-modal/create-environment-modal.component';

@Component({
  selector: 'app-infra-environment-constraint',
  templateUrl: './infra-environment-constraint.component.html',
  styleUrls: ['./infra-environment-constraint.component.scss']
})

export class InfraEnvironmentConstraintComponent implements OnInit {

  @Input() constraintKeys: string[] = [];
  @Input() cookbookConstraintArray: Array<CookbookConstraintGrid> = [];
  @Input() name_id: string;
  @Output() constraintValues: EventEmitter<CookbookConstraintGrid[]> = new EventEmitter();

  public conflictError = false;
  public operator_id = '';
  public operatorKeys: string[] = [];
  public secondFormGroup: FormGroup;
  public selectedCookbookNames: string[] = [];
  public nameKeys: string[] = [];

  constructor(
    private fb: FormBuilder
  ) {
    this.secondFormGroup = this.fb.group({
      version: ['', [Validators.required, Validators.pattern(Regex.patterns.VALID_VERSION)]]
    });

    // operators for Cookbook version
    this.operatorKeys = ['~>', '>=', '>', '=', '<', '<='];

    this.operator_id = this.operatorKeys[0];
  }

  ngOnInit() {
    this.conflictError = false;
    this.constraintKeys.forEach(element =>
      this.nameKeys.push(element)
    );
  }

  // Adding a new cookbook version
  public addCookbookVersion() {
    this.cookbookConstraintArray.unshift({
      id: this.cookbookConstraintArray.length + 1,
      name: this.name_id,
      operator: this.operator_id,
      version: this.secondFormGroup.controls['version'].value
    });
    this.constraintKeys.forEach((element, index) => {
      if (element === this.name_id) {
        this.selectedCookbookNames.unshift(element);
        this.constraintKeys.splice(index, 1);
      }
    });

    this.name_id = this.constraintKeys[0];
    this.operator_id = this.operatorKeys[0];

    this.secondFormGroup.controls.version.setValue('');
    this.constraintValues.emit(this.cookbookConstraintArray);
  }

  // Deleting a particular CookbookVersion
  public deleteCookbookVersionw(cookbookIndex: number, cookbookConstraint: CookbookConstraintGrid) {
    this.constraintKeys.push(cookbookConstraint.name);
    this.constraintKeys.sort((a, b) => a < b ? -1 : a > b ? 1 : 0);

    this.selectedCookbookNames.forEach((_, index) => {
      if (cookbookIndex === index) {
      this.selectedCookbookNames.splice(index, 1);

      }
    });

    this.name_id = this.constraintKeys[0];
    this.cookbookConstraintArray.splice(cookbookIndex, 1);
    this.constraintValues.emit(this.cookbookConstraintArray);
  }

  public handleInput(event: KeyboardEvent): void {
    if (this.isNavigationKey(event)) {
      return;
    }
    this.conflictError = false;
  }

  public handleEditName(newCookbookvalue: string, cookbookIndex: number) {
    let previousCookbookvalue: string;

    this.selectedCookbookNames.forEach((element, index) => {
      if (index === cookbookIndex) {
        previousCookbookvalue = element;
        if (newCookbookvalue && newCookbookvalue !== 'undefined') {
          this.selectedCookbookNames[cookbookIndex] = newCookbookvalue;
        }
      }
    });

    this.cookbookConstraintArray.forEach((_, index) => {
      if (index === cookbookIndex) {
        this.cookbookConstraintArray[cookbookIndex].name = newCookbookvalue;
      }

    });

    this.constraintKeys.forEach((element, index) => {
      if (element === newCookbookvalue) {
        this.constraintKeys.splice(index, 1);
      }
    });

    this.constraintKeys.push(previousCookbookvalue);
    this.constraintKeys.sort((a, b) => a < b ? -1 : a > b ? 1 : 0);
    this.name_id = this.constraintKeys[0];
    this.constraintValues.emit(this.cookbookConstraintArray);
  }

  public handleEditOperator(newCookbookOperator: string, cookbookIndex: number) {

    this.cookbookConstraintArray.forEach((_, index) => {
      if (index === cookbookIndex) {
        this.cookbookConstraintArray[cookbookIndex].operator = newCookbookOperator;
      }

    });
    this.constraintValues.emit(this.cookbookConstraintArray);
  }

  public handleEditVersion(newCookbookVersion: string, cookbookIndex: number) {
    this.cookbookConstraintArray.forEach((_, index) => {
      if (index === cookbookIndex) {
        this.cookbookConstraintArray[cookbookIndex].version = newCookbookVersion;
      }

    });
    this.constraintValues.emit(this.cookbookConstraintArray);
  }

  public isSelected(name: string) {
    return this.selectedCookbookNames.includes(name);
  }

  private isNavigationKey(event: KeyboardEvent): boolean {
    return event.key === 'Shift' || event.key === 'Tab';
  }

}
