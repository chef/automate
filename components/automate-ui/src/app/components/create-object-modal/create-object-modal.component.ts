import {
  Component, EventEmitter, Input, Output, OnInit, OnDestroy, OnChanges, SimpleChanges
} from '@angular/core';
import { FormGroup } from '@angular/forms';
import { Store } from '@ngrx/store';
import { Subject } from 'rxjs';
import { takeUntil, filter } from 'rxjs/operators';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { ChefSorters } from 'app/helpers/auth/sorter';
import { IdMapper } from 'app/helpers/auth/id-mapper';
import { Project, ProjectConstants } from 'app/entities/projects/project.model';
import { GetPolicies } from 'app/entities/policies/policy.actions';
import { allPolicies } from 'app/entities/policies/policy.selectors';
import { ResourceCheckedSection } from 'app/components/resource-dropdown/resource-dropdown.component';
import { Utilities } from 'app/helpers/utilities/utilities';

const INGEST_POLICY_ID = 'ingest-access';

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
  @Input() createForm: FormGroup; // NB: The form must contain 'name' and 'id' fields
  @Input() conflictErrorEvent: EventEmitter<boolean>; // TC: This element assumes 'id' is the
                                                      // only create field that can conflict.
  @Output() close = new EventEmitter();
  @Output() createClicked = new EventEmitter<Project[]>();

  public checkedProjectIDs: string[] = []; // resets project dropdown between modal openings
  public policies: ResourceCheckedSection[] = [];
  public modifyID = false; // Whether the edit ID form is open or not.
  public conflictError = false;
  public addPolicies = true;
  public projectsUpdatedEvent = new EventEmitter();
  public policiesUpdatedEvent = new EventEmitter();
  public UNASSIGNED_PROJECT_ID = ProjectConstants.UNASSIGNED_PROJECT_ID;

  private isDestroyed = new Subject<boolean>();

  constructor(private store: Store<NgrxStateAtom>) { }

  ngOnInit(): void {
    this.conflictErrorEvent.pipe(takeUntil(this.isDestroyed))
    .subscribe((isConflict: boolean) => {
      this.conflictError = isConflict;
      // Open the ID input on conflict so user can resolve it.
      this.modifyID = isConflict;
    });

    this.store.select(allPolicies)
      .pipe(filter(list => list.length > 0),
        takeUntil(this.isDestroyed))
      .subscribe(policies => {
        // separate into two sections, with ingest on top of chef-managed section
        const pols = ChefSorters.naturalSort(
          policies.filter(p => p.id !== INGEST_POLICY_ID), 'name');
        const customPolicies = pols.filter(p => p.type !== 'CHEF_MANAGED');
        const chefManagedPolicies = pols.filter(p => p.type === 'CHEF_MANAGED');
        const ingestPolicy = policies.find(p => p.id === INGEST_POLICY_ID);
        if (ingestPolicy) {
          chefManagedPolicies.unshift(ingestPolicy);
        }
        this.policies = [
          { title: 'Chef-managed', itemList: chefManagedPolicies},
          { title: 'Custom', itemList: customPolicies}
        ];
      });
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  ngOnChanges(changes: SimpleChanges): void {
    if (changes.visible && (changes.visible.currentValue as boolean)) {

      if (this.objectNoun === 'token') {
        this.store.dispatch(new GetPolicies()); // refresh in case of updates
      }

      this.checkedProjectIDs = []; // reset projects
      this.projectsUpdatedEvent.emit();

      this.policies.forEach(policy =>
        policy.itemList.forEach(p => p.checked = false)); // reset policies
      this.policiesUpdatedEvent.emit();

      if (this.createProjectModal) {
         // default to checked upon opening
        this.updatePolicyCheckbox(true);
      }
    }
  }

  onProjectDropdownClosing(projectsSelected: string[]): void {
    this.createForm.controls.projects.setValue(projectsSelected);
  }

  onPolicyDropdownClosing(policiesSelected: string[]): void {
    this.createForm.controls.policies.setValue(policiesSelected);
  }

  updatePolicyCheckbox(event: boolean): void {
    this.addPolicies = event;
    this.createForm.controls.addPolicies.setValue(this.addPolicies);
  }

  handleNameInput(event: KeyboardEvent): void {
    if (!this.modifyID && !Utilities.isNavigationKey(event)) {
      this.conflictError = false;
      this.createForm.controls.id.setValue(
        IdMapper.transform(this.createForm.controls.name.value.trim()));
    }
  }

  handleIDInput(event: KeyboardEvent): void {
    if (Utilities.isNavigationKey(event)) {
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

}
