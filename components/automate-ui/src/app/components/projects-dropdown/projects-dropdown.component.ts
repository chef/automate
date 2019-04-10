import {
  Component, EventEmitter, OnInit, Input, Output
} from '@angular/core';

import { find, orderBy } from 'lodash/fp';

@Component({
  selector: 'app-projects-dropdown',
  templateUrl: './projects-dropdown.component.html',
  styleUrls: ['./projects-dropdown.component.scss']
})
export class ProjectsDropdownComponent implements OnInit {

  active = false;
  showError = false;

  // The array of projects that you pass into the component. This array gets updated when a user
  // makes changes and is emitted to the parent component.
  @Input() projects = [];

  // Setting required to true means the dropdown will show an error if a user tries to close the
  // dropdown but hasn't selected any projects.
  @Input() required = false;

  // Setting disabled to true means the dropdown will be unusable and will have a grey background
  @Input() disabled = false;

  // Emits the array of projects to the parent component
  @Output() onSelection = new EventEmitter<any>();

  constructor() {
    this.projects = this.sortProjectsByName(this.projects);
  }

  ngOnInit() {}

  toggleDropdown(event) {
    event.stopPropagation();
    if (this.disabled) { return; }

    if (this.active) {
      this.closeColumnDropdown();
    } else {
      this.active = true;
    }
  }

  closeColumnDropdown() {
    const isValidSelection = this.validateSelection(this.required);

    if (!isValidSelection) {
      this.showError = true;
      return;
    } else {
      this.showError = false;
    }

    if (this.active) {
      this.onSelection.emit({ detail: this.projects });
      this.active = false;
    }
  }

  updateProjects(checked: boolean, index): void {
    this.projects[index].enabled = checked;
  }

  checkboxKeyPress(event, checked, index) {
    this.projects[index].enabled = !checked;
    event.target.setAttribute('checked', !checked);
  }

  moveFocus(event) {
    event.preventDefault();
    let element;

    if (event.key === 'ArrowUp') {
      element = event.target.previousElementSibling;
    } else if (event.key === 'ArrowDown') {
      element = event.target.nextElementSibling;
    }

    if (element == null)  {
      return;
    } else {
      element.focus();
    }
  }

  sortProjectsByName(projects) {
    return orderBy(['name'], ['asc'], projects);
  }

  validateSelection(isRequired) {
    const hasSelectedProjects = find(project => project.enabled, this.projects) ? true : false;

    return !isRequired || (isRequired && hasSelectedProjects);
  }
}
