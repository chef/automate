import { Component, EventEmitter, Input, Output, OnInit, OnDestroy } from '@angular/core';

import { ResourceChecked } from '../resource-dropdown/resource-dropdown.component';
import { ProjectConstants, Project } from 'app/entities/projects/project.model';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { assignableProjects } from 'app/services/projects-filter/projects-filter.selectors';
import { takeUntil } from 'rxjs/operators';
import { Subject } from 'rxjs';
import { ProjectsFilterOption } from 'app/services/projects-filter/projects-filter.reducer';

@Component({
  selector: 'app-projects-dropdown',
  templateUrl: './projects-dropdown.component.html',
  styleUrls: ['./projects-dropdown.component.scss']
})
export class ProjectsDropdownComponent implements OnInit, OnDestroy {

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

  public projects: Project[];

  private isDestroyed = new Subject<boolean>();

  constructor(private store: Store<NgrxStateAtom>) { }

  ngOnInit(): void {
    this.store.select(assignableProjects)
      .pipe(takeUntil(this.isDestroyed))
      .subscribe((assignable: ProjectsFilterOption[]) => {
        this.projects =
          assignable.map(p => {
            return <Project>{
              id: p.value,
              name: p.label,
              type: p.type
            };
          });
      });
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  onResourceChecked(project: ResourceChecked): void {
    this.onProjectChecked.emit(project);
  }

}
