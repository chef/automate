import { Component, EventEmitter, Input, Output } from '@angular/core';

import { ResourceCheckedMap, ResourceChecked } from '../resource-dropdown/resource-dropdown.component';
import { ProjectConstants } from 'app/entities/projects/project.model';

@Component({
  selector: 'app-projects-dropdown',
  templateUrl: './projects-dropdown.component.html',
  styleUrls: ['./projects-dropdown.component.scss']
})
export class ProjectsDropdownComponent {
  // The map of ResourceChecked by id. Any checked changes propagated via
  // onProjectChecked. Updates should be applied to parent component state.
  @Input() projects: ResourceCheckedMap = {};

  // Setting disabled to true means the dropdown will be unusable and will have a grey background
  @Input() disabled = false;

  // Used to re-synchronize summary label if the set of checked items has changed.
  // This optional input is needed only when re-displaying the project dropdown
  // for *additional* resources, as with the create-object-modal-component.
  // Other consumers, e.g. team-details.component use it only for a single resource.
  @Input() projectsUpdated: EventEmitter<boolean>;

  // Emits a project that changed as a result of a check or uncheck.
  @Output() onProjectChecked = new EventEmitter<ResourceChecked>();

  // Label to use when none are selected
  public noneSelectedLabel = ProjectConstants.UNASSIGNED_PROJECT_ID;

  // plural display name of resource
  public objectNounPlural = 'projects';

  onResourceChecked(project: ResourceChecked): void {
    this.onProjectChecked.emit(project);
  }

}
