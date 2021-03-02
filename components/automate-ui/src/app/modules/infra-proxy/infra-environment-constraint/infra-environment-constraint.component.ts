import {
  Component,
  OnInit,
  ViewChild,
  AfterViewInit,
  Input,
  Output,
  EventEmitter,
  QueryList,
  ElementRef,
  ViewChildren
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
export class InfraEnvironmentConstraintComponent implements OnInit, AfterViewInit {

  @Input() constraintName: string[] = [];
  public conflictError = false;
  @ViewChild(MatSelect) select: MatSelect;
  @Output() constraintValues: EventEmitter<DynamicGrid[]> = new EventEmitter();
  @ViewChildren('selectLang') nameSelects: QueryList<ElementRef<HTMLSelectElement>>;

  public secondFormGroup: FormGroup;
  public constraintKeys: string[] = [];
  public operatorKeys: string[] = [];
  public rowKeys: string[] = [];
  selectedLangs: string[] = [];
  public name_id = '';
  public operator_id = '';
  dynamicArray: Array<DynamicGrid> = [];

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
    this.constraintName.forEach(element =>

      this.rowKeys.push(element)
    );


    this.constraintName.forEach(element =>
      this.constraintKeys.push(element)
    );

    this.name_id = this.constraintKeys[0];

  }

  ngAfterViewInit() {
    this.select.overlayDir.positions = [
      {
        originX: 'center',
        originY: 'bottom',
        overlayX: 'center',
        overlayY: 'top'
      }
    ];
  }

  selected(value, i) {

    let previousValue, oldName;

    this.selectedLangs.forEach((element, index) => {
      if (index === i) {
        previousValue = element;
        if (value && value !== "undefined") this.selectedLangs[i] = value;

      }
    });

    this.dynamicArray.forEach((_, index) => {
      if (index === i) {
        oldName = this.dynamicArray[i].name;
        this.dynamicArray[i].name = value;
      }

    });



    this.constraintKeys.forEach((element, index) => {
      if (element === value) {
        this.constraintKeys.splice(index, 1);
        this.constraintName.splice(index, 1);

      }
    });

    this.constraintKeys.push(previousValue);
    this.constraintKeys.sort((a, b) => a < b ? -1 : a > b ? 1 : 0);
    this.name_id = this.constraintKeys[0];

    console.log('oldName' + oldName);

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

  public handleVersion(event, i) {

    this.dynamicArray.forEach((_, index) => {
      if (index === i) {
        this.dynamicArray[i].version = event;
      }

    });

    this.constraintValues.emit(this.dynamicArray);

  }

  public handleOperator(event, i) {

    this.dynamicArray.forEach((_, index) => {
      if (index === i) {
        this.dynamicArray[i].operator = event;
      }

    });
    this.constraintValues.emit(this.dynamicArray);
  }


  public handleName(event, i) {
    let oldName;

    this.dynamicArray.forEach((_, index) => {
      if (index === i) {
        oldName = this.dynamicArray[i].name;
        this.dynamicArray[i].name = event;
      }

    });

    console.log('oldName' + oldName);

    this.constraintKeys.forEach((element, index) => {
      if (element == event) {

        this.constraintKeys.splice(index, 1);
        this.constraintName.splice(index, 1);
      }

    });
    this.constraintKeys.sort((a, b) => a < b ? -1 : a > b ? 1 : 0);
    this.name_id = this.constraintKeys[0];
    this.constraintValues.emit(this.dynamicArray);


  }

  private isNavigationKey(event: KeyboardEvent): boolean {
    return event.key === 'Shift' || event.key === 'Tab';
  }

  addRow() {
    this.dynamicArray.push({
      id: this.dynamicArray.length + 1,
      name: this.name_id,
      operator: this.operator_id,
      version: this.secondFormGroup.controls['version'].value
    });
    this.constraintKeys.forEach((element, index) => {
      if (element == this.name_id) {
        this.selectedLangs.push(element);
        this.constraintKeys.splice(index, 1);
        this.constraintName.splice(index, 1);
      }

    });

    this.name_id = this.constraintKeys[0];
    this.operator_id = this.operatorKeys[0];

    this.secondFormGroup.controls.version.setValue('');
    this.constraintValues.emit(this.dynamicArray);
  }

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

}
