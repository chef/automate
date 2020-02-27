import { Component, OnInit, OnDestroy } from '@angular/core';
import { FormBuilder, FormGroup, Validators, FormControl } from '@angular/forms';
import { Store, select } from '@ngrx/store';
import { isEmpty, identity, xor, isNil } from 'lodash/fp';
import { combineLatest, Subject } from 'rxjs';
import { filter, pluck, takeUntil } from 'rxjs/operators';

import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { routeParams } from 'app/route.selectors';
import { Regex } from 'app/helpers/auth/regex';
import { pending, EntityStatus } from 'app/entities/entities';
import { GetToken, UpdateToken } from 'app/entities/api-tokens/api-token.actions';
import { apiTokenFromRoute, getStatus, updateStatus } from 'app/entities/api-tokens/api-token.selectors';
import { ApiToken } from 'app/entities/api-tokens/api-token.model';
import { isIAMv2 } from 'app/entities/policies/policy.selectors';
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

  public isIAMv2: boolean;
  public projects: ProjectCheckedMap = {};
  public unassigned = ProjectConstants.UNASSIGNED_PROJECT_ID;

  constructor(
    private store: Store<NgrxStateAtom>,
    fb: FormBuilder,
    private layoutFacade: LayoutFacadeService
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
    this.layoutFacade.showSidebar(Sidebar.Settings);

    this.store.pipe(
      select(isIAMv2),
      takeUntil(this.isDestroyed))
      .subscribe(latest => {
        this.isIAMv2 = latest;
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
      this.store.select(getStatus),
      this.store.select(apiTokenFromRoute)
    ]).pipe(
      filter(([status, token]) => status === EntityStatus.loadingSuccess && !isNil(token)),
      takeUntil(this.isDestroyed))
      .subscribe(([_, token]) => {
        this.token = { ...token };
        this.updateForm.controls.name.setValue(this.token.name);
        this.status = this.token.active ? 'active' : 'inactive';
        this.updateForm.controls.status.setValue(this.status);
        if (this.isIAMv2) {
          this.store.dispatch(new GetProjects());
        }
      });

    combineLatest([
      this.store.select(allProjects),
      this.store.select(getAllProjectStatus)
    ]).pipe(
      filter(([_, pStatus]: [Project[], EntityStatus]) => pStatus !== EntityStatus.loading),
      filter(() => !!this.token),
      takeUntil(this.isDestroyed))
      .subscribe(([allowedProjects, _]) => {
        this.projects = {};
        allowedProjects.forEach(p => {
            this.projects[p.id] = { ...p, checked: this.token.projects.includes(p.id)
            };
          });
      });

    this.store.pipe(
      select(updateStatus),
      takeUntil(this.isDestroyed),
      filter(state => this.saveInProgress && !pending(state)))
      .subscribe((state) => {
        this.saveInProgress = false;
        this.saveSuccessful = (state === EntityStatus.loadingSuccess);
        if (this.saveSuccessful) {
          this.updateForm.markAsPristine();
        }
      });
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
    const projects = Object.keys(this.projects).filter(id => this.projects[id].checked);
    this.store.dispatch(new UpdateToken({...this.token, name, active, projects }));
  }

  public get nameCtrl(): FormControl {
    return <FormControl>this.updateForm.controls.name;
  }

  // updates whether the project was checked or unchecked
  onProjectChecked(project: ProjectChecked): void {
    this.projects[project.id].checked = project.checked;

    // since the app-projects-dropdown is not a true form input (select)
    // we have to manage the form reactions
    if (this.noProjectsUpdated()) {
      this.updateForm.controls.projects.markAsPristine();
    } else {
      this.updateForm.controls.projects.markAsDirty();
    }
  }

  private noProjectsUpdated(): boolean {
    const projectsUpdated = xor(
      this.token.projects,
      Object.keys(this.projects).filter(id => this.projects[id].checked));
    return projectsUpdated.length === 0;
  }

  dropdownDisabled(): boolean {
    return isEmpty(this.projects) || this.saveInProgress;
  }
}
