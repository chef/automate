import { Component, EventEmitter, Input, Output, OnInit, OnDestroy, OnChanges, SimpleChanges } from '@angular/core';

import { ProjectConstants } from 'app/entities/projects/project.model';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { assignableProjects } from 'app/services/projects-filter/projects-filter.selectors';
import { takeUntil } from 'rxjs/operators';
import { Subject } from 'rxjs';
import { ProjectsFilterOption } from 'app/services/projects-filter/projects-filter.reducer';
import { ResourceChecked } from '../resource-dropdown/resource-dropdown.component';

@Component({
  selector: 'app-projects-dropdown',
  templateUrl: './projects-dropdown.component.html',
  styleUrls: ['./projects-dropdown.component.scss']
})
export class ProjectsDropdownComponent implements OnInit, OnDestroy, OnChanges {

  // Used to re-synchronize summary label if the set of checked items has changed.
  // This optional input is needed only when re-displaying the project dropdown
  // for *additional* resources, as with the create-object-modal-component.
  // Other consumers, e.g. team-details.component use it only for a single resource.
  @Input() projectsUpdated: EventEmitter<boolean>;

  @Input() checkedProjectIDs: string[] = [];

  // Emits checked set of ids upon completion.
  @Output() onDropdownClosing = new EventEmitter<string[]>();

  // Label to use when none are selected
  public noneSelectedLabel = ProjectConstants.UNASSIGNED_PROJECT_ID;

  public projects: ResourceChecked[];

  private isDestroyed = new Subject<boolean>();

  constructor(private store: Store<NgrxStateAtom>) { }

  ngOnInit(): void {
    this.store.select(assignableProjects)
      .pipe(takeUntil(this.isDestroyed))
      .subscribe((assignable: ProjectsFilterOption[]) => {
        this.projects =
          assignable.map(p => {
            return <ResourceChecked>{
              id: p.value,
              name: p.label,
              type: p.type,
              checked: this.checkedProjectIDs
                && this.checkedProjectIDs.includes(p.value)
            };
          });
      });
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  ngOnChanges(changes: SimpleChanges): void {
    if (changes.checkedProjectIDs && changes.checkedProjectIDs.currentValue && this.projects) {
      // Need to trigger OnChanges in ResourceDropdownComponent
      // so we cannot just update the checked property of the existing array elements.
      this.projects = this.projects
        .map(p => ({
          ...p,
          checked: (changes.checkedProjectIDs.currentValue as string[]).includes(p.id)
        }));
    }
  }

  onProjectDropdownClosing(ids: string[]): void {
    this.onDropdownClosing.emit(ids);
  }

}
