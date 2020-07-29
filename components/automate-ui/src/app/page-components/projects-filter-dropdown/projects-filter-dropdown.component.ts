import { Component, EventEmitter, Input, Output } from '@angular/core';
import { cloneDeep, isEqual } from 'lodash/fp';
import { ProjectsFilterOption } from 'app/services/projects-filter/projects-filter.reducer';
import { ProjectConstants } from 'app/entities/projects/project.model';

const { ALL_PROJECTS_LABEL } = ProjectConstants;

@Component({
  selector: 'app-projects-filter-dropdown',
  templateUrl: './projects-filter-dropdown.component.html',
  styleUrls: [ './projects-filter-dropdown.component.scss' ]
})
export class ProjectsFilterDropdownComponent {
  @Input() options: ProjectsFilterOption[] = [];

  @Input() selectionLabel = ALL_PROJECTS_LABEL;

  @Input() selectionCount = 0;

  @Input() selectionCountVisible = false;

  @Input() selectionCountActive = false;

  @Input() dropdownCaretVisible = false;

  @Input() filterVisible = false;

  @Output() onSelection = new EventEmitter<ProjectsFilterOption[]>();
  @Output() onOptionChange = new EventEmitter<ProjectsFilterOption[]>();

  editableOptions: ProjectsFilterOption[] = [];
  // Filtered options is merely a copy of the editable options
  // so they can be filtered while maintaining the actual options.
  filteredOptions: ProjectsFilterOption[] = [];
  // InitialFilters holds a boolean copy of the initial checked/unchecked state of filtered Options
  initialFilters: boolean[] = [];

  optionsEdited = false;

  dropdownActive = false;

  resetOptions() {
    if (!this.optionsEdited) {
      this.filteredOptions = this.editableOptions = cloneDeep(this.options);
      // Keep a reference to the filteredOptions in initialFilters
      // to check deep equality when a user unchecks/checks same button
      this.initialFilters = this.filteredOptions.map(option => option.checked);
    }
  }

  get filteredSelectedCount(): string {
    const count = this.filteredOptions.filter(option => option.checked).length;
    return count > 99 ? '99+' : count.toString();
  }

  handleFilterKeyUp(filterValue: string): void {
    this.filteredOptions = this.editableOptions
      .filter(option => option.label.toLowerCase().indexOf(filterValue.toLowerCase()) > -1);
  }

  handleLabelClick() {
    this.resetOptions();
    if (this.editableOptions.length > 1) {
      this.dropdownActive = !this.dropdownActive;
    }
  }

  handleEscape(): void {
    this.optionsEdited = false;
    this.resetOptions();
    this.dropdownActive = false;
    this.onOptionChange.emit(this.editableOptions);
  }

  handleOptionChange(event, label) {
    // sets the new state of the specific checkbox
    this.editableOptions
      .find(option => option.label === label).checked = event.detail;

    // Check for deep equality against new changes, disabling
    // apply button if changes are not new
    this.checkInitialEquality();
  }

  handleApplySelection() {
    this.dropdownActive = false;
    this.optionsEdited = false;
    this.onOptionChange.emit(this.editableOptions);
    this.onSelection.emit(this.editableOptions);
  }

  handleClearSelection() {
    // clear only entries visible by current filter
    // TODO: Micro-optimization: use a hash to convert this O(n^2) to O(n).
    this.editableOptions
      .filter(option => this.filteredOptions.find(o => o.label === option.label))
      .map(option => option.checked = false);

    this.checkInitialEquality();
  }

  handleArrowUp(event: KeyboardEvent) {
    event.preventDefault();

    const element = (event.target as Element).previousElementSibling;
    if (element) {
      (element as HTMLElement).focus();
    }
  }

  handleArrowDown(event: KeyboardEvent) {
    event.preventDefault();

    const element = (event.target as Element).nextElementSibling;
    if (element) {
      (element as HTMLElement).focus();
    }
  }

  checkInitialEquality() {
    // Check for equality against new changes, disabling
    // apply button if changes are not new

    // create a copy of checked/unchecked in most recently edited options
    const currentEdits: boolean[] = this.editableOptions.map(option => option.checked);

    this.optionsEdited = !isEqual(this.initialFilters, currentEdits);
  }
}
