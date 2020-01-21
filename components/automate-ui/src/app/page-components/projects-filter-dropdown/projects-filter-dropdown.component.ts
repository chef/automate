import { Component, EventEmitter, OnChanges, Input, Output } from '@angular/core';
import { cloneDeep } from 'lodash/fp';
import { ProjectsFilterOption } from 'app/services/projects-filter/projects-filter.reducer';
import { ProjectConstants } from 'app/entities/projects/project.model';

const { ALL_PROJECTS_LABEL } = ProjectConstants;

@Component({
  selector: 'app-projects-filter-dropdown',
  templateUrl: './projects-filter-dropdown.component.html',
  styleUrls: [ './projects-filter-dropdown.component.scss' ]
})
export class ProjectsFilterDropdownComponent implements OnChanges {
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
  filteredOptions: ProjectsFilterOption[] = [];

  filterValue = '';

  optionsEdited = false;

  dropdownActive = false;

  handleFilterKeyUp() {
    this.filteredOptions = this.editableOptions.filter(option =>
      option.label.toLowerCase().indexOf(this.filterValue.toLowerCase()) > -1
    );
  }

  ngOnChanges(changes) {
    if (changes.options && !this.optionsEdited) {
      this.resetOptions();
    }
  }

  resetOptions() {
    this.filteredOptions = this.editableOptions = cloneDeep(this.options);
    this.optionsEdited = false;
  }

  handleLabelClick() {
    if (this.editableOptions.length > 1) {
      this.dropdownActive = !this.dropdownActive;
      this.resetOptions();
    }
  }

  handleEscape(): void {
    this.resetOptions();
    this.filterValue = '';
    this.dropdownActive = false;
    this.onOptionChange.emit(this.editableOptions);
  }

  handleOptionChange(event, label) {
    this.editableOptions.find(option => {
      if (option.label === label) {
        option.checked = event.detail;
      }
    });
    this.optionsEdited = true;
    this.onOptionChange.emit(this.editableOptions);
  }

  handleApplySelection() {
    this.dropdownActive = false;
    this.optionsEdited = false;
    this.onSelection.emit(this.editableOptions);
  }

  handleClearSelection() {
    this.dropdownActive = false;
    this.optionsEdited = false;
    // uncheck all the options and then save
    this.editableOptions.map(option => option.checked = false);
    this.onSelection.emit(this.editableOptions);
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
}
