import { Component, EventEmitter, OnInit, OnDestroy } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { Store, select } from '@ngrx/store';
import { Observable, Subject, combineLatest } from 'rxjs';
import { filter, takeUntil, map } from 'rxjs/operators';
import { isNil } from 'lodash/fp';

import { LayoutFacadeService } from 'app/entities/layout/layout.facade';
import { DateTime } from 'app/helpers/datetime/datetime';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Regex } from 'app/helpers/auth/regex';
import { HttpStatus } from 'app/types/types';
import { ChefKeyboardEvent } from 'app/types/material-types';
import { loading, EntityStatus, pending } from 'app/entities/entities';
import { ChefSorters } from 'app/helpers/auth/sorter';
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
import { isIAMv2  } from 'app/entities/policies/policy.selectors';
import { assignableProjects } from 'app/services/projects-filter/projects-filter.selectors';
import { Project, ProjectConstants } from 'app/entities/projects/project.model';
import { ProjectsFilterOption } from 'app/services/projects-filter/projects-filter.reducer';

@Component({
  selector: 'app-api-tokens',
  templateUrl: './api-token-list.component.html',
  styleUrls: ['./api-token-list.component.scss']
})
export class ApiTokenListComponent implements OnInit, OnDestroy {
  public sortedApiTokens$: Observable<ApiToken[]>;
  public apiTokenCount$: Observable<number>;
  public deleteModalVisible = false;
  public tokenToDelete: ApiToken;
  public createModalVisible = false;
  public createTokenForm: FormGroup;
  public creatingToken = false;
  public conflictErrorEvent = new EventEmitter<boolean>();
  private isDestroyed = new Subject<boolean>();

  public isIAMv2$: Observable<boolean>;
  public dropdownProjects: Project[] = [];
  public unassigned = ProjectConstants.UNASSIGNED_PROJECT_ID;
  public readonly RFC2822 = DateTime.RFC2822;

  constructor(
    private store: Store<NgrxStateAtom>,
    fb: FormBuilder,
    private layoutFacade: LayoutFacadeService
  ) {
    store.pipe(
      select(apiTokenStatus),
      takeUntil(this.isDestroyed),
      map(loading)
    ).subscribe((isLoading) =>
      this.layoutFacade.ShowPageLoading(isLoading)
    );

    this.isIAMv2$ = store.select(isIAMv2);
    this.apiTokenCount$ = store.select(totalApiTokens);

    this.sortedApiTokens$ = store.pipe(
      select(allApiTokens),
      map(tokens => ChefSorters.naturalSort(tokens, 'name')));

    this.createTokenForm = fb.group({
      // Must stay in sync with error checks in create-object-modal.component.html
      name: ['', Validators.required],
      id: ['',
        [Validators.required, Validators.pattern(Regex.patterns.ID), Validators.maxLength(64)]],
      projects: [[]]
    });
  }

  ngOnInit() {
    this.layoutFacade.showSidebar('settings');
    this.store.dispatch(new GetAllTokens());

    this.store.select(assignableProjects)
      .pipe(takeUntil(this.isDestroyed))
      .subscribe((assignable: ProjectsFilterOption[]) => {
        this.dropdownProjects = assignable.map(p => {
          return <Project>{
            id: p.value,
            name: p.label,
            type: p.type
          };
        });
      });

    this.store.pipe(
      select(saveStatus),
      takeUntil(this.isDestroyed),
      filter(state => this.createModalVisible && !pending(state)))
      .subscribe(state => {
        this.creatingToken = false;
        if (state === EntityStatus.loadingSuccess) {
          this.closeCreateModal();
        }
      });

    combineLatest([
      this.store.select(saveStatus),
      this.store.select(saveError)
    ]).pipe(
      takeUntil(this.isDestroyed),
      filter(() => this.createModalVisible),
      filter(([state, error]) => state === EntityStatus.loadingFailure && !isNil(error)))
      .subscribe(([_, error]) => {
        if (error.status === HttpStatus.CONFLICT) {
          this.conflictErrorEvent.emit(true);
        } else {
          // Close the modal on any error other than conflict and display in banner.
          this.closeCreateModal();
        }
      });
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public closeDeleteModal(): void {
    this.deleteModalVisible = false;
  }

  public startTokenDelete($event: ChefKeyboardEvent, token: ApiToken): void {
    if ($event.isUserInput) {
      this.tokenToDelete = token;
      this.deleteModalVisible = true;
    }
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

  }

  public toggleActive($event: ChefKeyboardEvent, token: ApiToken): void {
    if ($event.isUserInput) {
      this.store.dispatch(new ToggleTokenActive(token));
    }
  }

  public openCreateModal(): void {
    this.createModalVisible = true;
    this.resetCreateModal();
  }

  public closeCreateModal(): void {
    this.createModalVisible = false;
    this.resetCreateModal();
  }

  public notifyCopy($event: ChefKeyboardEvent): void {
    if ($event.isUserInput) {
      this.store.dispatch(new CreateNotification({
        type: Type.info,
        message: 'API Token copied to clipboard.'
      }));
    }
  }

  private resetCreateModal(): void {
    this.creatingToken = false;
    this.createTokenForm.reset();
    this.conflictErrorEvent.emit(false);
  }
}
