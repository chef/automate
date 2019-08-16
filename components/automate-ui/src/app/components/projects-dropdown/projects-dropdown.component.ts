import {
  Component, EventEmitter, Input, Output, OnChanges
} from '@angular/core';

import { ProjectConstants, Project } from 'app/entities/projects/project.model';

const { UNASSIGNED_PROJECT_ID } = ProjectConstants;

export interface ProjectChecked extends Project {
  checked: boolean;
}

@Component({
  selector: 'app-projects-dropdown',
  templateUrl: './projects-dropdown.component.html',
  styleUrls: ['./projects-dropdown.component.scss']
})
export class ProjectsDropdownComponent implements OnChanges {
  // The map of ProjectChecked by id. Any checked changes propogated via
  // onProjectChecked. Updates should be applied to parent component state.
  @Input() projects: { [id: string]: ProjectChecked } = {};

  // Setting disabled to true means the dropdown will be unusable and will have a grey background
  @Input() disabled = false;

  // Emits a project that changed as a result of a check or uncheck.
  @Output() onProjectChecked = new EventEmitter<ProjectChecked>();

  active = false;
  label = UNASSIGNED_PROJECT_ID;

  constructor() { }

  projectsArray(): ProjectChecked[] {
    return Object.values(this.projects);
  }

  ngOnChanges() {
    this.updateLabel();
  }

  toggleDropdown(event: MouseEvent): void {
    event.stopPropagation();
    if (this.disabled) { return; }

    this.active = !this.active;
  }

  projectChecked(checked: boolean, project: ProjectChecked): void {
    project.checked = checked;
    this.updateLabel();
    this.onProjectChecked.emit(project);
  }

  closeDropdown(): void {
    if (this.active) {
      this.active = false;
    }
  }

  moveFocus(event: KeyboardEvent): void {
    event.preventDefault();
    let nextElement: HTMLElement;

    const targetElement = <Element>event.target;
    if (event.key === 'ArrowUp') {
      nextElement = <HTMLElement>targetElement.previousElementSibling;
    } else if (event.key === 'ArrowDown') {
      nextElement = <HTMLElement>targetElement.nextElementSibling;
    }

    if (nextElement == null)  {
      return;
    } else {
      nextElement.focus();
    }
  }

  updateLabel(): void {
    const checkedProjects = this.projectsArray().filter(p => p.checked);
    switch (checkedProjects.length) {
      case 1: {
        const onlyProject = checkedProjects[0];
        this.label = onlyProject.name;
        break;
      }
      case 0: {
        this.label = UNASSIGNED_PROJECT_ID;
        break;
      }
      default: {
        this.label = `${checkedProjects.length} projects`;
        break;
      }
    }
  }
}
