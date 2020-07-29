import { Component, EventEmitter, Input, Output, OnInit, OnDestroy, OnChanges, SimpleChanges } from '@angular/core';
import { Store } from '@ngrx/store';
import { Subject } from 'rxjs';
import { takeUntil } from 'rxjs/operators';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { ProjectConstants } from 'app/entities/projects/project.model';
import { assignableProjects } from 'app/services/projects-filter/projects-filter.selectors';
import { ProjectsFilterOption } from 'app/services/projects-filter/projects-filter.reducer';
import { ResourceCheckedSection } from 'app/components/resource-dropdown/resource-dropdown.component';

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

  public projects: ResourceCheckedSection[];

  private isDestroyed = new Subject<boolean>();

  constructor(private store: Store<NgrxStateAtom>) { }

  ngOnInit(): void {
    this.store.select(assignableProjects)
      .pipe(takeUntil(this.isDestroyed))
      .subscribe((assignable: ProjectsFilterOption[]) => {
        this.projects = [{
          itemList: assignable.map(p => {
            return {
              id: p.value,
              name: p.label,
              type: p.type,
              checked: this.checkedProjectIDs
                && this.checkedProjectIDs.includes(p.value)
            };
          })
        }];
      });
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  ngOnChanges(changes: SimpleChanges): void {
    if (changes.checkedProjectIDs) {
      // cannot proceed unless we have received the available projects list from the back end
      if (this.projects) {
        // Need to trigger OnChanges in ResourceDropdownComponent
        // so we cannot just update the checked property of the existing array elements.
        this.projects = [{
          itemList: this.projects[0].itemList
            .map(p => ({
              ...p,
              checked: (changes.checkedProjectIDs.currentValue as string[]).includes(p.id)
            }))
        }];
      }
    }
  }

  onProjectDropdownClosing(ids: string[]): void {
    this.onDropdownClosing.emit(ids);
  }

}
