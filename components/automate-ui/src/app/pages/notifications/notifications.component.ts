import { Component, OnInit, OnDestroy, EventEmitter } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatOptionSelectionChange } from '@angular/material/core/option';

import { Observable, combineLatest, Subject } from 'rxjs';
import { Store, select } from '@ngrx/store';
import { filter, takeUntil, map } from 'rxjs/operators';
import { isNil } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Regex } from 'app/helpers/auth/regex';
import { EntityStatus, pending } from 'app/entities/entities';
import { HttpStatus } from 'app/types/types';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { NotificationRule, ServiceActionType } from 'app/entities/notification_rules/notification_rule.model';
import { SortDirection } from '../../types/types';
// import { RulesService } from '../../services/rules/rules.service';
import { TelemetryService } from '../../services/telemetry/telemetry.service';
import { NotificationRuleRequests } from 'app/entities/notification_rules/notification_rule.requests';
import {
  allRules,
  saveStatus,
  saveError
} from 'app/entities/notification_rules/notification_rule.selectors';
import {
  GetNotificationRules,
  DeleteNotificationRule,
  CreateNotificationRule
} from 'app/entities/notification_rules/notification_rule.action';

export interface FieldDirection {
  name: SortDirection;
  alert_type: SortDirection;
  failure_type: SortDirection;
  webhook_url: SortDirection;
}

enum UrlTestState {
  Inactive,
  Loading,
  Success,
  Failure
}

@Component({
  selector: 'app-notifications',
  templateUrl: './notifications.component.html',
  styleUrls: ['./notifications.component.scss']
})
export class NotificationsComponent implements OnInit, OnDestroy {
  rules$: Observable<NotificationRule[]>;
  errorLoading = false;
  currentPage = 1;
  pageSize = 10;
  sortField: string;
  sortDir: FieldDirection;
  direction = 'none';
  permissionDenied = false; // not currently used
  // This is exposed here to allow the component HTML access to ServiceActionType
  serviceActionType = ServiceActionType;

  public createModalVisible = false;
  public createNotificationForm: FormGroup;
  public creatingNotification = false;
  public sendingNotification = false;
  public conflictErrorEvent = new EventEmitter<boolean>();
  public notificationToDelete: NotificationRule;
  public deleteModalVisible = false;
  public hookStatus = UrlTestState.Inactive;
  public notificationObj = new NotificationRule('', '', null, '', null, '', false);
  private isDestroyed = new Subject<boolean>();

  public targetKeys: string[];
  public alertTypeKeys: string[];

  constructor(
    private store: Store<NgrxStateAtom>,
    private fb: FormBuilder,
    private layoutFacade: LayoutFacadeService,
    private notificationRuleRequests: NotificationRuleRequests,
    private telemetryService: TelemetryService
  ) {
     this.rules$ = store.pipe(select(allRules));
    this.createNotificationForm = this.fb.group({
      // Must stay in sync with error checks in create-notification-modal.component.html
      name: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      ruleType: ['ComplianceFailure', []],
      targetType: [ServiceActionType.WEBHOOK, []],

      // Note that URL here may be FQDN -or- IP!
      targetUrl: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      username: [[]],
      password: [[]]
    });

    this.targetKeys = this.notificationObj.getTargetTypeKeys();
    this.alertTypeKeys = this.notificationObj.getAlertTypeKeys();
   }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Settings);
    this.store.dispatch(new GetNotificationRules());
    this.store.pipe(
      select(saveStatus),
      takeUntil(this.isDestroyed),
      filter(state => this.createModalVisible && !pending(state)))
      .subscribe(state => {
        this.creatingNotification = false;
        if (state === EntityStatus.loadingSuccess) {
          this.closeCreateModal();
          this.hookStatus = UrlTestState.Inactive;
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
          this.hookStatus = UrlTestState.Inactive;
        }
      });

    this.rules$.subscribe(rules => {
        this.sendCountToTelemetry(rules);
      },
      error => {
        if (error.status === 403) {
          this.permissionDenied = true;
        } else  {
          this.errorLoading = true;
        }
      }
    );

    this.resetSortDir();
    this.toggleSort('name');
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public openCreateModal(): void {
    this.createModalVisible = true;
    this.resetCreateModal();
  }

  public closeCreateModal(): void {
    this.createModalVisible = false;
    this.resetCreateModal();
  }

  public createNotification(): void {
    this.creatingNotification = true;
    this.notificationObj.name = this.createNotificationForm.value.name.trim();
    this.notificationObj.targetUrl = this.createNotificationForm.value.targetUrl.trim();
    const username: string = this.createNotificationForm.value.username || '';
    const password: string = this.createNotificationForm.value.password || '';
    this.store.dispatch(new CreateNotificationRule(this.notificationObj, username, password));
  }

  toggleSort(field: string) {
    if (field === this.sortField) {
      // when sorting is inverted for the currently sorted column
      switch (this.direction) {
        case 'none':
          this.direction = this.sortDir[field] = 'asc';
          break;
        case 'asc':
          this.direction = this.sortDir[field] = 'desc';
          break;
        case 'desc':
        default:
          this.direction = this.sortDir[field] = 'none';
      }
    } else {
      // when sorting a different column than the currently sorted one
      this.resetSortDir();
    }
    if (this.sortDir[field] === 'none') {
      this.resetSortDir();
      this.toggleSort('name');
    } else {
    this.sortField = field;
    this.updateSort(field, this.sortDir[field]);
    }
  }

  sortIcon(field: string): string {
    if (field === this.sortField) {
      return 'sort-' + this.sortDir[field];
    } else {
      return 'none';
    }
  }

  public startNotificationDelete($event: MatOptionSelectionChange, rule: NotificationRule): void {
    if ($event.isUserInput) {
      this.notificationToDelete = rule;
      this.deleteModalVisible = true;
    }
  }

  public deleteNotification(): void {
    this.closeDeleteModal();
    this.store.dispatch(new DeleteNotificationRule({ rule: this.notificationToDelete}));
  }

  public closeDeleteModal(): void {
    this.deleteModalVisible = false;
  }

  private resetCreateModal(): void {
    this.hookStatus = UrlTestState.Inactive;
    this.creatingNotification = false;
    this.createNotificationForm.reset();
    this.conflictErrorEvent.emit(false);
  }

  private updateSort(field: string, direction: string) {
    this.direction = direction;
    this.rules$ = this.rules$.pipe(map(rules => {
      let sortedRules: NotificationRule[] = [];
      if (field === 'name') {
        sortedRules = rules.sort((r1: NotificationRule, r2: NotificationRule) => {
          return r1.name.localeCompare(r2.name);
        });
      } else if (field === 'alert_type') {
        sortedRules = rules.sort((r1: NotificationRule, r2: NotificationRule) => {
          return r1.targetType.localeCompare(r2.targetType);
        });
      } else if (field === 'failure_type') {
        sortedRules = rules.sort((r1: NotificationRule, r2: NotificationRule) => {
          return r1.AlertTypeLabels[r1.ruleType].localeCompare(r2.AlertTypeLabels[r2.ruleType]);
        });
      } else if (field === 'webhook_url') {
        sortedRules = rules.sort((r1: NotificationRule, r2: NotificationRule) => {
          return r1.targetUrl.localeCompare(r2.targetUrl);
        });
      }

      if (direction === 'asc') {
        return sortedRules;
      } else {
        return sortedRules.reverse();
      }
    }));
  }

  private resetSortDir(): void {
    this.sortDir = {
      name: 'asc',
      alert_type: 'asc',
      failure_type: 'asc',
      webhook_url: 'asc'
    };
  }

  private sendCountToTelemetry(rules: NotificationRule[]) {
    const ruleCount = rules.reduce(
      (acc, rule: NotificationRule) => {
        if (rule.ruleType === 'CCRFailure') {
          acc['ccrRuleCount'] += 1;
        } else if (rule.ruleType === 'ComplianceFailure') {
          acc['complianceRuleCount'] += 1;
        }

        if (rule.targetType === ServiceActionType.SLACK) {
          acc['slackRuleCount'] += 1;
        } else if (rule.targetType === ServiceActionType.WEBHOOK) {
          acc['webhookRuleCount'] += 1;
        }
        return acc;
      },
      {
        totalRuleCount: rules.length,
        ccrRuleCount: 0,
        complianceRuleCount: 0,
        slackRuleCount: 0,
        webhookRuleCount: 0
      });

    this.telemetryService.track('notificationRuleCount', ruleCount);
  }

  public updateTargetType(): void {
    if (this.createNotificationForm.value.targetType === 'ServiceNowAlert') {
      this.createNotificationForm.get('username').setValidators([
        Validators.required
      ]);
      this.createNotificationForm.get('password').setValidators([
        Validators.required
      ]);
    } else {
      this.createNotificationForm.get('username').clearValidators();
      this.createNotificationForm.get('password').clearValidators();
    }
  }

  public sendTestForNotification(): void {
    this.sendingNotification = true;
    this.hookStatus = UrlTestState.Loading;
    const targetUrl: string =  this.createNotificationForm.controls['targetUrl'].value;
    const targetUsername: string = this.createNotificationForm.controls['username'].value;
    const targetPassword: string = this.createNotificationForm.controls['password'].value;
    if (targetUrl && targetUsername && targetPassword) {
      this.notificationRuleRequests.testHookWithUsernamePassword(targetUrl,
        targetUsername, targetPassword).subscribe(
          () => this.revealUrlStatus(UrlTestState.Success),
          () => this.revealUrlStatus(UrlTestState.Failure)
        );
    } else {
      this.notificationRuleRequests.testHookWithNoCreds(targetUrl)
        .subscribe(
          () => this.revealUrlStatus(UrlTestState.Success),
          () => this.revealUrlStatus(UrlTestState.Failure)
        );
    }
    this.sendingNotification = false;
  }

  private revealUrlStatus(status: UrlTestState) {
    this.hookStatus = status;
  }

  // TODO - this was common in all three uses, but I'm not sure this is the best
  // way to do it - do we really need to refresh after the server confirms the action
  // successful? Seem it should be possible to update the local model with the
  // changes and have that trigger view updates?
  refreshRules() {
    // this.rules$ = this.service.fetchRules();
    this.rules$.subscribe(rules => {
      this.sendCountToTelemetry(rules);
      this.updateSort(this.sortField, this.sortDir[this.sortField]);
    });
  }

}
