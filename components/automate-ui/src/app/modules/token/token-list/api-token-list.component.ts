import { Component, EventEmitter, OnInit, OnDestroy } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatOptionSelectionChange } from '@angular/material/core';
import { Store, select } from '@ngrx/store';
import { Observable, Subject, combineLatest } from 'rxjs';
import { filter, takeUntil, map } from 'rxjs/operators';
import { isNil } from 'lodash/fp';

import { LayoutFacadeService, Sidebar } from '../../../entities/layout/layout.facade';
import { NgrxStateAtom } from '../../../ngrx.reducers';
import { Regex } from '../../../helpers/auth/regex';
import { HttpStatus } from '../../../types/types';
import { loading, EntityStatus, pending } from '../../../entities/entities';
import { ChefSorters } from '../../../helpers/auth/sorter';
import { Type } from '../../../entities/notifications/notification.model';
import { CreateNotification } from '../../../entities/notifications/notification.actions';
import { ApiToken } from '../../../entities/api-tokens/api-token.model';
import {
  allApiTokens, totalApiTokens, apiTokenStatus
} from '../../../entities/api-tokens/api-token.selectors';
import {
  GetAllTokens, DeleteToken, ToggleTokenActive
} from '../../../entities/api-tokens/api-token.actions';
import { CreateToken } from '../../../entities/api-tokens/api-token.actions';
import { saveStatus, saveError } from '../../../entities/api-tokens/api-token.selectors';
import { ProjectConstants } from '../../../entities/projects/project.model';
import { AddPolicyMembers, PolicyMembersMgmtPayload } from '../../../entities/policies/policy.actions';
import { stringToMember } from '../../../entities/policies/policy.model';
import { TelemetryService } from '../../../services/telemetry/telemetry.service';

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

  public unassigned = ProjectConstants.UNASSIGNED_PROJECT_ID;

  constructor(
    private store: Store<NgrxStateAtom>,
    fb: FormBuilder,
    private layoutFacade: LayoutFacadeService,
    private telemetryService: TelemetryService
  ) {
    store.pipe(
      select(apiTokenStatus),
      takeUntil(this.isDestroyed),
      map(loading)
    ).subscribe((isLoading) =>
      this.layoutFacade.ShowPageLoading(isLoading)
    );

    this.apiTokenCount$ = store.select(totalApiTokens);

    this.sortedApiTokens$ = store.pipe(
      select(allApiTokens),
      map(tokens => ChefSorters.naturalSort(tokens, 'name')));

    this.createTokenForm = fb.group({
      // Must stay in sync with error checks in create-object-modal.component.html
      name: ['', Validators.required],
      id: ['',
        [Validators.required, Validators.pattern(Regex.patterns.ID), Validators.maxLength(64)]],
      projects: [[]],
      policies: [[]]
    });
  }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Settings);
    this.store.dispatch(new GetAllTokens());

    this.store.pipe(
      select(saveStatus),
      takeUntil(this.isDestroyed),
      filter(state => this.createModalVisible && !pending(state)))
      .subscribe(state => {
        this.creatingToken = false;
        if (state === EntityStatus.loadingSuccess) {
          this.assignPolicies();
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

  public startTokenDelete($event: MatOptionSelectionChange, token: ApiToken): void {
    if ($event.isUserInput) {
      this.tokenToDelete = token;
      this.deleteModalVisible = true;
    }
  }

  public deleteToken(): void {
    this.closeDeleteModal();
    this.store.dispatch(new DeleteToken(this.tokenToDelete));
    this.telemetryService.track('Settings_APItokens_Delete');
  }

  public createToken(): void {
    this.creatingToken = true;
    const tok = {
      id: this.createTokenForm.controls['id'].value,
      name: this.createTokenForm.controls['name'].value.trim(),
      projects: this.createTokenForm.controls['projects'].value
    };
    this.store.dispatch(new CreateToken(tok));
    this.telemetryService.track('Settings_APItokens_Create');
  }

  private assignPolicies(): void {
    const member = stringToMember(`token:${this.createTokenForm.controls['id'].value}`);
    const policies: string[] = this.createTokenForm.controls['policies'].value || [];
    policies.forEach(id => this.store.dispatch(new AddPolicyMembers(<PolicyMembersMgmtPayload>{
      id,
      members: [member]
    })));
  }

  public toggleActive($event: MatOptionSelectionChange, token: ApiToken): void {
    if ($event.isUserInput) {
      this.store.dispatch(new ToggleTokenActive(token));
    }
    if (token && token.active) {
      this.telemetryService.track('Settings_APItokens_ToggleInActive');
    } else {
      this.telemetryService.track('Settings_APItokens_ToggleActive');
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

  public notifyCopy($event: MatOptionSelectionChange): void {
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
