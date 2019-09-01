import { Component, OnInit, OnDestroy } from '@angular/core';
import { FormBuilder, FormGroup, Validators, FormControl } from '@angular/forms';
import { Store, select } from '@ngrx/store';
import { isEmpty, identity, xor } from 'lodash/fp';
import { combineLatest, Subject, Observable } from 'rxjs';
import { filter, map, pluck, takeUntil } from 'rxjs/operators';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { routeParams } from 'app/route.selectors';
import { Regex } from 'app/helpers/auth/regex';
import { loading, EntityStatus } from 'app/entities/entities';
import { GetToken, UpdateToken } from 'app/entities/api-tokens/api-token.actions';
import { apiTokenFromRoute, updateStatus } from 'app/entities/api-tokens/api-token.selectors';
import { ApiToken } from 'app/entities/api-tokens/api-token.model';
import { atLeastV2p1 } from 'app/entities/policies/policy.selectors';
import { Project, ProjectConstants } from 'app/entities/projects/project.model';
import { GetProjects } from 'app/entities/projects/project.actions';
import {
  allProjects,
  getAllStatus as getAllProjectStatus
} from 'app/entities/projects/project.selectors';
import {
  ProjectChecked,
  ProjectCheckedMap
} from 'app/components/projects-dropdown/projects-dropdown.component';

type TokenStatus = 'active' | 'inactive';
type TokenTabName = 'details';

@Component({
  selector: 'app-api-token-details',
  templateUrl: './api-token-details.component.html',
  styleUrls: ['./api-token-details.component.scss']
})
export class ApiTokenDetailsComponent implements OnInit, OnDestroy {
  public tabValue: TokenTabName = 'details';
  public token: ApiToken;
  public status: TokenStatus;
  private isDestroyed = new Subject<boolean>();
  public updateForm: FormGroup;
  public saveInProgress = false;
  public saveSuccessful = false;

  public atLeastV2p1$: Observable<boolean>;
  public projects: ProjectCheckedMap = {};
  public unassigned = ProjectConstants.UNASSIGNED_PROJECT_ID;

  constructor(
    private store: Store<NgrxStateAtom>,
    fb: FormBuilder
  ) {
    const initialStatus: TokenStatus = 'active';
    this.updateForm = fb.group({
      // Must stay in sync with error checks in api-token-details.component.html
      name: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      status: [initialStatus],
      projects: [[]]
    });
  }

  ngOnInit(): void {
    this.atLeastV2p1$ = this.store.select(atLeastV2p1);

    this.store.pipe(
      select(apiTokenFromRoute),
      filter(identity),
      takeUntil(this.isDestroyed))
      .subscribe((token) => {
        this.token = { ...token };
        this.updateForm.controls.name.setValue(this.token.name);
        this.status = this.token.active ? 'active' : 'inactive';
        this.updateForm.controls.status.setValue(this.status);
        this.store.dispatch(new GetProjects());
      });

    this.store.pipe(
      select(routeParams),
      pluck('id'),
      filter(identity),
      takeUntil(this.isDestroyed))
      .subscribe((id: string) => {
        this.store.dispatch(new GetToken({ id }));
      });

    combineLatest([
      this.store.select(allProjects),
      this.store.select(getAllProjectStatus)
    ]).pipe(
      takeUntil(this.isDestroyed),
      filter(([_, pStatus]: [Project[], EntityStatus]) => pStatus !== EntityStatus.loading),
      filter(() => !!this.token),
      map(([allowedProjects, _]) => {
        this.projects = {};
        allowedProjects
          .forEach(p => {
            this.projects[p.id] = { ...p, checked: this.token.projects.includes(p.id)
            };
          });
      }))
      .subscribe();
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public saveToken(): void {
    this.saveSuccessful = false;
    this.saveInProgress = true;
    const name: string = this.updateForm.controls.name.value.trim();
    const active = <TokenStatus>this.updateForm.controls.status.value === 'active';
    const projects: string[] = this.updateForm.controls.projects.value;
    const token: ApiToken = { ...this.token, name, active, projects };
    this.store.dispatch(new UpdateToken({ token }));

    const pendingSave = new Subject<boolean>();
    this.store.pipe(
      select(updateStatus),
      filter(identity),
      takeUntil(pendingSave))
      .subscribe((state) => {
        if (!loading(state)) {
          pendingSave.next(true);
          pendingSave.complete();
          this.saveInProgress = false;
          this.saveSuccessful = (state === EntityStatus.loadingSuccess);
          if (this.saveSuccessful) {
            this.updateForm.markAsPristine();
          }
        }
      });
  }

  public get nameCtrl(): FormControl {
    return <FormControl>this.updateForm.controls.name;
  }

  // updates whether the project was checked or unchecked
  onProjectChecked(project: ProjectChecked): void {
    this.projects[project.id].checked = project.checked;
    const projectsSelected = Object.values(this.projects).filter(p => p.checked);

    // since the app-projects-dropdown is not a true form input (select)
    // we have to manage the form reactions
    this.updateForm.controls.projects.setValue(projectsSelected.map(p => p.id));
    this.updateForm.controls.projects.markAsDirty();
  }

  private noProjectsUpdated(): boolean {
    const projectsUpdated = xor(
      this.token.projects,
      Object.keys(this.projects).filter(id => this.projects[id].checked));
    return projectsUpdated.length === 0;
  }

  // Special handling needed due to the projects dropdown being inside the form.
  // Once the project list changes, the form remains dirty
  // so cannot check the form's dirty bit.
  // TODO: Figure a way to make the <app-projects-dropdown> a proper form control
  // so it can be managed bo FormControlDirective like other form fields.
  get formPristine(): boolean {
    return !this.updateForm.controls.name.dirty &&
      !this.updateForm.controls.status.dirty &&
      this.noProjectsUpdated();
  }

  dropdownDisabled(): boolean {
    return isEmpty(this.projects) || this.saveInProgress;
  }
}
