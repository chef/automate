import { Component,
  EventEmitter,
  Input,
  Output,
  OnInit,
  OnChanges,
  SimpleChanges
} from '@angular/core';
import { FormGroup } from '@angular/forms';
import { IdMapper } from 'app/helpers/auth/id-mapper';
import { ProjectConstants } from 'app/entities/projects/project.model';
import { Utilities } from 'app/helpers/utilities/utilities';

@Component({
  selector: 'app-create-org-modal',
  templateUrl: './create-org-modal.component.html',
  styleUrls: ['./create-org-modal.component.scss']
})
export class CreateOrgModalComponent implements OnInit, OnChanges {
  @Input() visible = false;
  @Input() creating = false;
  @Input() conflictErrorEvent: EventEmitter<boolean>;
  @Output() close = new EventEmitter();
  @Output() createClicked = new EventEmitter();
  @Input() createForm: FormGroup;

  public conflictError = false;
  public modifyID = false; // Whether the edit ID form is open or not.
  public projectsUpdatedEvent = new EventEmitter();
  public checkedProjectIDs: string[] = []; // resets project dropdown between modal openings
  public UNASSIGNED_PROJECT_ID = ProjectConstants.UNASSIGNED_PROJECT_ID;

  ngOnInit() {
    this.conflictErrorEvent.subscribe((isConflict: boolean) => {
      this.conflictError = isConflict;
      // Open the ID input on conflict so user can resolve it.
      this.modifyID = isConflict;
    });
  }

  ngOnChanges(changes: SimpleChanges): void {
    if (changes.visible && (changes.visible.currentValue as boolean)) {
      this.checkedProjectIDs = []; // reset projects
      this.projectsUpdatedEvent.emit();
    }
  }

  onProjectDropdownClosing(projectsSelected: string[]): void {
    this.createForm.controls.projects.setValue(projectsSelected);
  }

  handleNameInput(event: KeyboardEvent): void {
    if (!this.modifyID && !Utilities.isNavigationKey(event)) {
      this.conflictError = false;
      this.createForm.controls.id.setValue(
        IdMapper.transform(this.createForm.controls.name.value.trim()));
    }
  }

  public handleInput(event: KeyboardEvent): void {
    if (Utilities.isNavigationKey(event)) {
      return;
    }
    this.conflictError = false;
  }

  closeEvent(): void {
    this.modifyID = false;
    this.checkedProjectIDs = [];
    this.close.emit();
  }

  createServerOrg(): void {
    this.createClicked.emit();
    this.checkedProjectIDs = [];
  }

}
