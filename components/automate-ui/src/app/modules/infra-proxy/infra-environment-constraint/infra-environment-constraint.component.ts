import {
  Component,
  OnInit,
  Input,
  Output,
  EventEmitter
} from '@angular/core';

import { FormGroup } from '@angular/forms';
import { Utilities } from 'app/helpers/utilities/utilities';

import { CookbookConstraintGrid } from '../create-environment-modal/create-environment-modal.component';

@Component({
  selector: 'app-infra-environment-constraint',
  templateUrl: './infra-environment-constraint.component.html',
  styleUrls: ['./infra-environment-constraint.component.scss']
})

export class InfraEnvironmentConstraintComponent implements OnInit {

  @Input() constraintKeys: string[] = [];
  @Input() cookbookConstraints: Array<CookbookConstraintGrid> = [];
  @Input() name_id: string;
  @Input() nameKeys: string[] = [];
  @Input() selectedCookbookNames: string[] = [];
  @Input() constraintFormGroup: FormGroup;
  @Output() constraintValues: EventEmitter<CookbookConstraintGrid[]> = new EventEmitter();

  public conflictError = false;
  public operator_id = '';
  public operators: string[] = [];

  constructor(
  ) {
    this.operators = [
      '~>', // A pessimistic will find the upper limit version number within the range
      // specified by the minor version number or patch version number.
      '>=', '>', '=', '<', '<='];
    this.operator_id = this.operators[0];
  }

  ngOnInit() {
    this.conflictError = false;
  }

  public addCookbookVersion() {
    this.cookbookConstraints.unshift({
      id: this.cookbookConstraints.length + 1,
      name: this.name_id,
      operator: this.operator_id,
      version: this.constraintFormGroup.controls['version'].value
    });
    this.constraintKeys.forEach((element, index) => {
      if (element === this.name_id) {
        this.selectedCookbookNames.unshift(element);
        this.constraintKeys.splice(index, 1);
      }
    });

    this.name_id = this.constraintKeys[0];
    this.operator_id = this.operators[0];

    this.constraintFormGroup.controls.version.setValue('');
    this.toUpdateCookbookData(this.cookbookConstraints);
  }

  public deleteCookbookVersion(cookbookIndex: number, cookbookConstraint: CookbookConstraintGrid) {
    this.constraintKeys.push(cookbookConstraint.name);
    this.constraintKeys.sort((a, b) => a < b ? -1 : a > b ? 1 : 0);

    this.selectedCookbookNames.splice(cookbookIndex, 1);

    this.name_id = this.constraintKeys[0];
    this.cookbookConstraints.splice(cookbookIndex, 1);
    this.toUpdateCookbookData(this.cookbookConstraints);
  }

  public handleInput(event: KeyboardEvent): void {
    if (Utilities.isNavigationKey(event)) {
      return;
    }
    this.conflictError = false;
  }

  public handleEditName(newName: string, cookbookIndex: number) {
    let previousName: string;

    previousName = this.toUpdateSelectedCookbookNames(newName, cookbookIndex, previousName);

    this.cookbookConstraints[cookbookIndex].name = newName;

    this.toUpdateCookbookKeys(newName);

    this.constraintKeys.push(previousName);
    this.constraintKeys.sort((a, b) => a < b ? -1 : a > b ? 1 : 0);
    this.name_id = this.constraintKeys[0];
    this.toUpdateCookbookData(this.cookbookConstraints);
  }

  public handleEditOperator(newCookbookOperator: string, cookbookIndex: number) {

    this.cookbookConstraints[cookbookIndex].operator = newCookbookOperator;

    this.toUpdateCookbookData(this.cookbookConstraints);
  }

  public handleEditVersion(newCookbookVersion: string, cookbookIndex: number) {

    this.cookbookConstraints[cookbookIndex].version = newCookbookVersion;

    this.toUpdateCookbookData(this.cookbookConstraints);
  }

  public isSelected(name: string) {
    return this.selectedCookbookNames.includes(name);
  }

  private toUpdateCookbookData(cookbookConstraints: CookbookConstraintGrid[]) {
    this.constraintValues.emit(cookbookConstraints);
  }

  private toUpdateCookbookKeys(newName: string) {
    this.constraintKeys.forEach((element, index) => {
      if (element === newName) {
        this.constraintKeys.splice(index, 1);
      }
    });
  }

  private toUpdateSelectedCookbookNames(newName: string, cookbookIndex: number,
    previousName: string) {
    this.selectedCookbookNames.forEach((element, index) => {
      if (index === cookbookIndex) {
        previousName = element;
        if (newName) {
          this.selectedCookbookNames[cookbookIndex] = newName;
        }
      }
    });
    return previousName;
  }

}
