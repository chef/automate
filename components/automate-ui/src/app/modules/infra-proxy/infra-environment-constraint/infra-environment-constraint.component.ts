import {
  Component,
  OnInit,
  ViewChild,
  Input,
  Output,
  EventEmitter
} from '@angular/core';

import { FormBuilder, Validators, FormGroup } from '@angular/forms';
import { Regex } from 'app/helpers/auth/regex';

import { MatSelect } from '@angular/material/select';
import { DynamicGrid } from '../create-environment-modal/create-environment-modal.component';

@Component({
  selector: 'app-infra-environment-constraint',
  templateUrl: './infra-environment-constraint.component.html',
  styleUrls: ['./infra-environment-constraint.component.scss']
})

export class InfraEnvironmentConstraintComponent implements OnInit {

  @Input() constraintKeys: string[] = [];
  @Input() dynamicArray: Array<DynamicGrid> = [];
  @Input() name_id: string;
  @Output() constraintValues: EventEmitter<DynamicGrid[]> = new EventEmitter();
  @ViewChild(MatSelect) select: MatSelect;

  public conflictError = false;
  public operator_id = '';
  public operatorKeys: string[] = [];
  public secondFormGroup: FormGroup;
  public selectedLangs: string[] = [];
  public rowKeys: string[] = [];

  constructor(
    private fb: FormBuilder
  ) {
    this.secondFormGroup = this.fb.group({
      version: ['', [Validators.required, Validators.pattern(Regex.patterns.VALID_VERSION)]]
    });

    this.operatorKeys = ['~>', '>=', '>', '=', '<', '<='];

    this.operator_id = this.operatorKeys[0];
  }

  ngOnInit() {

    this.conflictError = false;
    this.constraintKeys.forEach(element =>

      this.rowKeys.push(element)
    );
  }

  selected(value, i) {

    let previousValue;

    this.selectedLangs.forEach((element, index) => {
      if (index === i) {
        previousValue = element;
        if (value && value !== 'undefined') {
          this.selectedLangs[i] = value;
        }
      }
    });

    this.dynamicArray.forEach((_, index) => {
      if (index === i) {
        this.dynamicArray[i].name = value;
      }

    });

    this.constraintKeys.forEach((element, index) => {
      if (element === value) {
        this.constraintKeys.splice(index, 1);
      }
    });

    this.constraintKeys.push(previousValue);
    this.constraintKeys.sort((a, b) => a < b ? -1 : a > b ? 1 : 0);
    this.name_id = this.constraintKeys[0];
    this.constraintValues.emit(this.dynamicArray);
  }

  isSelected(lang: string) {
    return this.selectedLangs.includes(lang);
  }

  public handleInput(event: KeyboardEvent): void {
    if (this.isNavigationKey(event)) {
      return;
    }
    this.conflictError = false;
  }

  // Handle the version change
  public handleVersion(event, i) {
    this.dynamicArray.forEach((_, index) => {
      if (index === i) {
        this.dynamicArray[i].version = event;
      }

    });
    this.constraintValues.emit(this.dynamicArray);
  }

  // Handle the operator change
  public handleOperator(event, i) {

    this.dynamicArray.forEach((_, index) => {
      if (index === i) {
        this.dynamicArray[i].operator = event;
      }

    });
    this.constraintValues.emit(this.dynamicArray);
  }

  // Handle the name change
  public handleName(event, i) {
    this.dynamicArray.forEach((_, index) => {
      if (index === i) {
        this.dynamicArray[i].name = event;
      }

    });

    this.constraintKeys.forEach((element, index) => {
      if (element === event) {
        this.constraintKeys.splice(index, 1);
      }

    });
    this.constraintKeys.sort((a, b) => a < b ? -1 : a > b ? 1 : 0);
    this.name_id = this.constraintKeys[0];
    this.constraintValues.emit(this.dynamicArray);
  }

  // Adding a new constraint
  addRow() {
    this.dynamicArray.unshift({
      id: this.dynamicArray.length + 1,
      name: this.name_id,
      operator: this.operator_id,
      version: this.secondFormGroup.controls['version'].value
    });
    this.constraintKeys.forEach((element, index) => {
      if (element === this.name_id) {
        this.selectedLangs.push(element);
        this.constraintKeys.splice(index, 1);
      }
    });

    this.name_id = this.constraintKeys[0];
    this.operator_id = this.operatorKeys[0];

    this.secondFormGroup.controls.version.setValue('');
    this.constraintValues.emit(this.dynamicArray);
  }

  // Deleting a particular constraint
  deleteRow(index, dynamic) {
    this.constraintKeys.push(dynamic.name);
    this.constraintKeys.sort((a, b) => a < b ? -1 : a > b ? 1 : 0);

    this.selectedLangs.forEach((_, indes) => {
      if (index === indes) {
      this.selectedLangs.splice(indes, 1);

      }
    });

    this.name_id = this.constraintKeys[0];
    this.dynamicArray.splice(index, 1);
    this.constraintValues.emit(this.dynamicArray);

  }

  private isNavigationKey(event: KeyboardEvent): boolean {
    return event.key === 'Shift' || event.key === 'Tab';
  }

}
