import {
  Component, EventEmitter, Input, Output
} from '@angular/core';

import { ProjectConstants, Project } from 'app/entities/projects/project.model';

const { UNASSIGNED_PROJECT_ID } = ProjectConstants;

@Component({
  selector: 'app-projects-dropdown',
  templateUrl: './projects-dropdown.component.html',
  styleUrls: ['./projects-dropdown.component.scss']
})
export class ProjectsDropdownComponent {

  // The array of projects that you pass into the component.
  @Input() projects: Array<Project> = [];

  // Setting required to true means the dropdown will show an error if a user tries to close the
  // dropdown but hasn't selected any projects.
  @Input() required = false;

  // Setting disabled to true means the dropdown will be unusable and will have a grey background
  @Input() disabled = false;

  // Emits the array of projects to the parent component
  @Output() onSelection = new EventEmitter<Array<Project>>();

  active = false;
  showError = false;
  label = UNASSIGNED_PROJECT_ID;

  // Map of projects currently selected
  selectedProjects: { [id: string]: Project } = {};

  constructor() { }

  // If you want to mark some projects as pre-selected, use @ViewChild.
  // See team-details.component.ts for an example.
  public updateSelectedProjects(project: Project, enabled: boolean): void {
    console.log('update');
    console.log(project);
    console.log(enabled);
    console.log(this.selectedProjects);
    if (enabled) {
      this.selectedProjects[project.id] = project;
    } else {
      delete this.selectedProjects[project.id];
    }
    this.updateLabel();
  }

  projectSelected(project: Project): boolean {
    return this.selectedProjects[project.id] !== undefined;
  }

  toggleDropdown(event: MouseEvent): void {
    event.stopPropagation();
    if (this.disabled) { return; }

    if (this.active) {
      this.closeDropdown();
    } else {
      this.active = true;
    }
  }

  closeDropdown(): void {
    const isValidSelection = this.validateSelection(this.required);

    if (!isValidSelection) {
      this.showError = true;
      return;
    } else {
      this.showError = false;
    }

    if (this.active) {
      const result = <Project[]>Object.values(this.selectedProjects);
      const resultProjects = result.map(p => <Project>{
        id: p.id,
        name: p.name,
        type: p.type
      });
      this.onSelection.emit(resultProjects);
      this.active = false;
    }
  }

  updateProjects(checked: boolean, index: number): void {
    this.updateSelectedProjects(this.projects[index], checked);
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

  private updateLabel(): void {
    const selectedLen = Object.keys(this.selectedProjects).length;
    switch (selectedLen) {
      case 1: {
        const onlyProject = <Project>Object.values(this.selectedProjects)[0];
        this.label = onlyProject.name;
        break;
      }
      case 0: {
        this.label = UNASSIGNED_PROJECT_ID;
        break;
      }
      default: {
        this.label = `${selectedLen} projects`;
        break;
      }
    }
  }

  private validateSelection(isRequired): boolean {
    return !isRequired || (isRequired && Object.keys(this.selectedProjects).length > 0);
  }
}
