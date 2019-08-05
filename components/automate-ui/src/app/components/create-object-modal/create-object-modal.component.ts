import { Component, EventEmitter, Input, Output, OnInit } from '@angular/core';
import { FormGroup } from '@angular/forms';
import { IdMapper } from 'app/helpers/auth/id-mapper';
import { Project } from 'app/entities/projects/project.model';

@Component({
  selector: 'app-create-object-modal',
  templateUrl: './create-object-modal.component.html',
  styleUrls: ['./create-object-modal.component.scss']
})
export class CreateObjectModalComponent implements OnInit {
  @Input() visible = false;
  @Input() creating = false;
  @Input() objectNoun: string;
  @Input() showProjectsDropdown = false;
  @Input() projectsAssignable: Project[] = [];
  @Input() createForm: FormGroup; // NB: The form must contain 'name' and 'id' fields
  @Input() conflictErrorEvent: EventEmitter<boolean>; // TC: This element assumes 'id' is the
                                                      // only create field that can conflict.
  @Output() close = new EventEmitter();
  @Output() createClicked = new EventEmitter<Project[]>();

  // Whether the edit ID form is open or not.
  public modifyID = false;
  public conflictError = false;
  private projectsSelected: Project[] = [];

  ngOnInit(): void {
    this.conflictErrorEvent.subscribe((isConflict: boolean) => {
      this.conflictError = isConflict;
      // Open the ID input on conflict so user can resolve it.
      this.modifyID = true;
    });
  }

  public handleNameInput(event: KeyboardEvent): void {
    if (!this.modifyID && !this.isNavigationKey(event)) {
      this.conflictError = false;
      this.createForm.controls['id'].setValue(
        IdMapper.transform(this.createForm.controls['name'].value.trim()));
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
    this.createClicked.emit(this.projectsSelected);
  }

  private isNavigationKey(event: KeyboardEvent): boolean {
    return event.key === 'Shift' || event.key === 'Tab';
  }

  projectsDropdownAction(projects) {
    this.projectsSelected = projects;
  }
}
