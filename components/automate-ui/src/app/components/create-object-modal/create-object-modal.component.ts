import {
  Component, EventEmitter, Input, Output, OnInit, OnChanges, SimpleChanges
} from '@angular/core';
import { FormGroup } from '@angular/forms';
import { hasIn } from 'lodash/fp';

import { IdMapper } from 'app/helpers/auth/id-mapper';
import { Project } from 'app/entities/projects/project.model';
import {
  ProjectChecked,
  ProjectCheckedMap
} from 'app/components/projects-dropdown/projects-dropdown.component';

@Component({
  selector: 'app-create-object-modal',
  templateUrl: './create-object-modal.component.html',
  styleUrls: ['./create-object-modal.component.scss']
})
export class CreateObjectModalComponent implements OnInit, OnChanges {
  @Input() visible = false;
  @Input() creating = false;
  @Input() objectNoun: string;
  @Input() showProjectsDropdown = false;
  @Input() assignableProjects: Project[] = [];
  @Input() createForm: FormGroup; // NB: The form must contain 'name' and 'id' fields
  @Input() conflictErrorEvent: EventEmitter<boolean>; // TC: This element assumes 'id' is the
                                                      // only create field that can conflict.
  @Output() close = new EventEmitter();
  @Output() createClicked = new EventEmitter<Project[]>();

  public projects: ProjectCheckedMap = {};

  // Whether the edit ID form is open or not.
  public modifyID = false;
  public conflictError = false;

  ngOnInit(): void {
    this.conflictErrorEvent.subscribe((isConflict: boolean) => {
      this.conflictError = isConflict;
      // Open the ID input on conflict so user can resolve it.
      this.modifyID = true;
    });
  }

  ngOnChanges(changes: SimpleChanges): void {
    // if a new list of projects to populate dropdown with is passed in we update the dropdown
    const checked = false;
    if (hasIn('assignableProjects.currentValue', changes)) {
      this.projects = {};
      changes.assignableProjects.currentValue.forEach((proj: Project) =>
        this.projects[proj.id] = { ...proj, checked });
    }
  }

  onProjectChecked(project: ProjectChecked): void {
    this.projects[project.id].checked = project.checked;
    const projectsSelected = Object.keys(this.projects).filter(id => this.projects[id].checked);
    this.createForm.controls.projects.setValue(projectsSelected);
  }

  dropdownDisabled(): boolean {
    return Object.values(this.projects).length === 0;
  }

  public handleNameInput(event: KeyboardEvent): void {
    if (!this.modifyID && !this.isNavigationKey(event)) {
      this.conflictError = false;
      this.createForm.controls.id.setValue(
        IdMapper.transform(this.createForm.controls.name.value.trim()));
    }
  }

  public handleIDInput(event: KeyboardEvent): void {
    if (this.isNavigationKey(event)) {
      return;
    }
    this.conflictError = false;
  }

  closeEvent(): void {
    this.modifyID = false;
    this.close.emit();
  }

  createObject(): void {
    this.createClicked.emit();
  }

  private isNavigationKey(event: KeyboardEvent): boolean {
    return event.key === 'Shift' || event.key === 'Tab';
  }
}
