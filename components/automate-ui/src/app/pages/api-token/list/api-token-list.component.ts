import { Component, EventEmitter, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { Store, select } from '@ngrx/store';
import { Observable, Subject } from 'rxjs';
import { filter, takeUntil, map } from 'rxjs/operators';
import { identity } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Regex } from 'app/helpers/auth/regex';
import { HttpStatus } from 'app/types/types';
import { loading, EntityStatus } from 'app/entities/entities';
import { Type } from 'app/entities/notifications/notification.model';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { ApiToken } from 'app/entities/api-tokens/api-token.model';
import {
  allApiTokens, totalApiTokens, apiTokenStatus
} from 'app/entities/api-tokens/api-token.selectors';
import {
  GetAllTokens, DeleteToken, ToggleTokenActive
} from 'app/entities/api-tokens/api-token.actions';
import { CreateToken } from 'app/entities/api-tokens/api-token.actions';
import { saveStatus, saveError } from 'app/entities/api-tokens/api-token.selectors';
import { iamMajorVersion, iamMinorVersion } from 'app/entities/policies/policy.selectors';
import { IAMMajorVersion, IAMMinorVersion } from 'app/entities/policies/policy.model';
import { assignableProjects } from 'app/services/projects-filter/projects-filter.selectors';
import { Project, ProjectConstants } from 'app/entities/projects/project.model';
import { ProjectsFilterOption } from 'app/services/projects-filter/projects-filter.reducer';

@Component({
  selector: 'app-api-tokens',
  templateUrl: './api-token-list.component.html',
  styleUrls: ['./api-token-list.component.scss']
})
export class ApiTokenListComponent implements OnInit {
  public loading$: Observable<boolean>;
  public sortedApiTokens$: Observable<ApiToken[]>;
  public apiTokenCount$: Observable<number>;
  public deleteModalVisible = false;
  public tokenToDelete: ApiToken;
  public createModalVisible = false;
  public createTokenForm: FormGroup;
  public creatingToken = false;
  public conflictErrorEvent = new EventEmitter<boolean>();

  public iamMajorVersion$: Observable<IAMMajorVersion>;
  public iamMinorVersion$: Observable<IAMMinorVersion>;
  public isMajorV1 = true;
  public isMinorV1 = false;
  public dropdownProjects: Project[] = [];
  public unassigned = ProjectConstants.UNASSIGNED_PROJECT_ID;

  private isDestroyed = new Subject<boolean>();

  constructor(
    private store: Store<NgrxStateAtom>,
    private router: Router,
    fb: FormBuilder) {
    this.loading$ = store.pipe(select(apiTokenStatus), map(loading));
    this.sortedApiTokens$ = store.pipe(
      select(allApiTokens),
      map(tokens => tokens.sort(
        (a, b) => {
          // See https://stackoverflow.com/a/38641281 for these options
          const opts = { numeric: true, sensitivity: 'base' };
          // Sort by name then by cased-name, since no other field
          // is useful as a secondary sort; this ensures stable sort with
          // respect to case, so 'a' always comes before 'A'.
          return a.name.localeCompare(b.name, undefined, opts)
            || a.name.localeCompare(b.name, undefined, {numeric: true});
        }
      )));
    this.apiTokenCount$ = store.pipe(select(totalApiTokens));

    this.createTokenForm = fb.group({
      // Must stay in sync with error checks in create-object-modal.component.html
      name: ['', Validators.required],
      id: ['',
        [Validators.required, Validators.pattern(Regex.patterns.ID), Validators.maxLength(64)]],
      projects: [[]] 
    });
    this.iamMajorVersion$ = store.pipe(select(iamMajorVersion));
    this.iamMinorVersion$ = store.pipe(select(iamMinorVersion));
  }

  ngOnInit() {
    this.store.dispatch(new GetAllTokens());

    this.iamMajorVersion$
      .pipe(takeUntil(this.isDestroyed))
      .subscribe((majorVersion) => {
        if (majorVersion === null) { return; }
        this.isMajorV1 = majorVersion === 'v1';
      });

    this.iamMinorVersion$
      .pipe(takeUntil(this.isDestroyed))
      .subscribe((minorVersion) => {
        if (minorVersion === null) { return; }
        this.isMinorV1 = minorVersion === 'v1';
      });

    this.store.select(assignableProjects)
      .subscribe((assignable: ProjectsFilterOption[]) => {
        this.dropdownProjects = assignable.map(p => {
          return <Project>{
            id: p.value,
            name: p.label,
            type: p.type
          };
        });
      });
  }

  public closeDeleteModal(): void {
    this.deleteModalVisible = false;
  }

  public startTokenDelete(token: ApiToken): void {
    this.tokenToDelete = token;
    this.deleteModalVisible = true;
  }

  public deleteToken(): void {
    this.closeDeleteModal();
    this.store.dispatch(new DeleteToken(this.tokenToDelete));
  }

  public createToken(): void {
    this.creatingToken = true;
    const tok = {
      id: this.createTokenForm.controls['id'].value,
      name: this.createTokenForm.controls['name'].value.trim(),
      projects: this.createTokenForm.controls.projects.value
    };
    this.store.dispatch(new CreateToken(tok));

    const pendingCreate = new Subject<boolean>();
    this.store.pipe(
      select(saveStatus),
      filter(identity),
      takeUntil(pendingCreate))
      .subscribe((state) => {
        if (!loading(state)) {
          pendingCreate.next(true);
          pendingCreate.complete();
          this.creatingToken = false;
          if (state === EntityStatus.loadingSuccess) {
            this.closeCreateModal();
            this.router.navigate(['/settings', 'tokens', tok.id]);
          }
          if (state === EntityStatus.loadingFailure) {
            const pendingCreateError = new Subject<boolean>();
            this.store.pipe(
              select(saveError),
              filter(identity),
              takeUntil(pendingCreateError))
              .subscribe((error) => {
                pendingCreateError.next(true);
                pendingCreateError.complete();
                if (error.status === HttpStatus.CONFLICT) {
                  this.conflictErrorEvent.emit(true);
                // Close the modal on any error other than conflict and display in banner.
                } else {
                  this.closeCreateModal();
                }
            });
          }
        }
      });
  }

  public toggleActive(token: ApiToken): void {
    this.store.dispatch(new ToggleTokenActive(token));
  }

  public openCreateModal(): void {
    this.createModalVisible = true;
    this.resetCreateModal();
  }

  public closeCreateModal(): void {
    this.createModalVisible = false;
    this.resetCreateModal();
  }

  public notifyCopy(): void {
    this.store.dispatch(new CreateNotification({
      type: Type.info,
      message: 'API Token copied to clipboard.'
    }));
  }

  resetCreateModal(): void {
    this.creatingToken = false;
    this.createTokenForm.reset();
  }
}
