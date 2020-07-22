import { Component, OnChanges, Input, Output, EventEmitter, SimpleChanges } from '@angular/core';
import { DesktopColumnOption, DesktopColumnOptionUpdate } from 'app/entities/desktop/desktop.model';
import { cloneDeep, isEqual } from 'lodash/fp';

@Component({
  selector: 'app-insight-attributes-dropdown',
  templateUrl: './insight-attributes-dropdown.component.html',
  styleUrls: ['./insight-attributes-dropdown.component.scss']
})
export class InsightAttributesDropdownComponent implements OnChanges {

  @Input() options: DesktopColumnOption[] = [];
  @Input() max = 5;
  @Input() saveAsDefault = false;

  @Output() onUpdate = new EventEmitter<DesktopColumnOptionUpdate>();
  @Output() onToggleMenu: EventEmitter<null> = new EventEmitter();

  editableOptions: DesktopColumnOption[] = [];
  editableSaveAsDefault = false;

  ngOnChanges(changes: SimpleChanges) {
    if (changes.options) {
      this.resetOptions();
    }

    this.editableSaveAsDefault = this.saveAsDefault;
  }

  resetOptions() {
    this.editableOptions = cloneDeep(this.options);
  }

  handleOptionChange(event, index) {
    this.editableOptions[index].checked = event.detail;
  }

  handleSaveAsDefaultChange(event) {
    this.editableSaveAsDefault = event.detail;
  }

  handleUpdate() {
    this.onUpdate.emit({
      options: this.editableOptions,
      saveAsDefault: this.editableSaveAsDefault
    });
    this.onToggleMenu.emit();
  }

  handleCancel() {
    this.resetOptions();
    this.editableSaveAsDefault = this.saveAsDefault;
    this.onToggleMenu.emit();
  }

  get disableSubmit(): boolean {
    return this.saveDefaultChanged || this.optionsChanged ? false : true;
  }

  get saveDefaultChanged(): boolean {
    return this.editableSaveAsDefault !== this.saveAsDefault;
  }

  get optionsChanged(): boolean {
    return !isEqual(this.editableOptions.sort(), this.options.sort());
  }

  get activeOptions(): DesktopColumnOption[]  {
    return this.editableOptions.filter(o => o.checked);
  }

  get maxOptionsActive(): boolean {
    return this.activeOptions.length === this.max;
  }

}
