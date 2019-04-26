import { Component, EventEmitter, OnChanges, Input, Output } from '@angular/core';
import { cloneDeep } from 'lodash/fp';
import { ProjectsFilterOption } from 'app/services/projects-filter/projects-filter.reducer';

@Component({
  selector: 'app-projects-filter-dropdown',
  templateUrl: './projects-filter-dropdown.component.html',
  styleUrls: [ './projects-filter-dropdown.component.scss' ]
})
export class ProjectsFilterDropdownComponent implements OnChanges {
  @Input() options: ProjectsFilterOption[] = [];

  @Input() selectionLabel = 'All Resources';

  @Input() selectionCount = 0;

  @Input() selectionCountVisible = false;

  @Input() selectionCountActive = false;

  @Output() onSelection = new EventEmitter<ProjectsFilterOption[]>();

  editableOptions: ProjectsFilterOption[] = [];

  optionsEdited = false;

  dropdownActive = false;

  ngOnChanges(changes) {
    if (changes.options) {
      this.resetOptions();
    }
  }

  resetOptions() {
    this.editableOptions = cloneDeep(this.options);
    this.optionsEdited = false;
  }

  handleLabelClick() {
    if (this.editableOptions.length > 1) {
      this.dropdownActive = !this.dropdownActive;
      this.resetOptions();
    }
  }

  handleEscape() {
    this.dropdownActive = false;
    this.resetOptions();
  }

  handleOptionChange(event, index) {
    this.editableOptions[index].checked = event.detail;
    this.optionsEdited = true;
  }

  handleApplyClick() {
    this.dropdownActive = false;
    this.optionsEdited = false;
    this.onSelection.emit(this.editableOptions);
  }

  handleArrowUp(event) {
    event.preventDefault();

    const element = event.target.previousElementSibling;
    if (element) {
      element.focus();
    }
  }

  handleArrowDown(event) {
    event.preventDefault();

    const element = event.target.nextElementSibling;
    if (element) {
      element.focus();
    }
  }
}
