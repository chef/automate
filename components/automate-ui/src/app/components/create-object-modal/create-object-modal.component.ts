import {
  Component, EventEmitter, Input, Output, OnInit, OnDestroy, OnChanges, SimpleChanges
} from '@angular/core';
import { FormGroup } from '@angular/forms';

import { IdMapper } from 'app/helpers/auth/id-mapper';
import { Project } from 'app/entities/projects/project.model';
import {
  ProjectChecked,
  ProjectCheckedMap
} from 'app/components/projects-dropdown/projects-dropdown.component';
import { Subject } from 'rxjs';
import { takeUntil } from 'rxjs/operators';

@Component({
  selector: 'app-create-object-modal',
  templateUrl: './create-object-modal.component.html',
  styleUrls: ['./create-object-modal.component.scss']
})
export class CreateObjectModalComponent implements OnInit, OnDestroy, OnChanges {
  @Input() visible = false;
  @Input() creating = false;
  @Input() objectNoun: string;
  @Input() createProjectModal = false;
  @Input() assignableProjects: Project[] = [];
  @Input() createForm: FormGroup; // NB: The form must contain 'name' and 'id' fields
  @Input() conflictErrorEvent: EventEmitter<boolean>; // TC: This element assumes 'id' is the
                                                      // only create field that can conflict.
  @Output() close = new EventEmitter();
  @Output() createClicked = new EventEmitter<Project[]>();

  public projects: ProjectCheckedMap = {};
  public modifyID = false; // Whether the edit ID form is open or not.
  public conflictError = false;
  public projectsUpdatedEvent = new EventEmitter();

  private isDestroyed = new Subject<boolean>();

  ngOnInit(): void {
    this.conflictErrorEvent.pipe(takeUntil(this.isDestroyed))
    .subscribe((isConflict: boolean) => {
      this.conflictError = isConflict;
      // Open the ID input on conflict so user can resolve it.
      this.modifyID = isConflict;
    });
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  ngOnChanges(changes: SimpleChanges): void {
    // update project dropdown if list changes
    if (changes.assignableProjects) {
      this.projects = {};
      changes.assignableProjects.currentValue.forEach((proj: Project) =>
        this.projects[proj.id] = { ...proj, checked: false });
    }
    // clear checked projects when opening
    if (changes.visible && (changes.visible.currentValue as boolean)) {
      Object.values(this.projects).forEach(p => p.checked = false);
      this.projectsUpdatedEvent.emit();
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

  handleNameInput(event: KeyboardEvent): void {
    if (!this.modifyID && !this.isNavigationKey(event)) {
      this.conflictError = false;
      this.createForm.controls.id.setValue(
        IdMapper.transform(this.createForm.controls.name.value.trim()));
    }
  }

  handleIDInput(event: KeyboardEvent): void {
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
