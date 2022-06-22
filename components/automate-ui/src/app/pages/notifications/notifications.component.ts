import { Component, OnInit, OnDestroy, EventEmitter } from '@angular/core';
import { MatOptionSelectionChange } from '@angular/material/core/option';
import { Observable, Subject } from 'rxjs';
import { Store, select } from '@ngrx/store';
import { takeUntil, map } from 'rxjs/operators';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { NotificationRule, ServiceActionType } from 'app/entities/notification_rules/notification_rule.model';
import { SortDirection } from '../../types/types';
import { TelemetryService } from '../../services/telemetry/telemetry.service';
import {
  allRules
} from 'app/entities/notification_rules/notification_rule.selectors';
import {
  GetNotificationRules,
  DeleteNotificationRule
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

  public notificationToDelete: NotificationRule;
  public deleteModalVisible = false;
  public hookStatus = UrlTestState.Inactive;
  public notificationObj = new NotificationRule('', '', null, '', null, '', false);
  private isDestroyed = new Subject<boolean>();

  public openNotificationModal = new EventEmitter<void>();

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService,
    private telemetryService: TelemetryService
  ) {
    this.rules$ = store.pipe(select(allRules));
  }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Settings);
    this.store.dispatch(new GetNotificationRules());

    this.rules$.pipe(
      takeUntil(this.isDestroyed))
      .subscribe(rules => {
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
    this.openNotificationModal.emit();
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
    this.store.dispatch(new DeleteNotificationRule(this.notificationToDelete));
    this.telemetryService.track('Settings_Notifications_Delete');
  }

  public closeDeleteModal(): void {
    this.deleteModalVisible = false;
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

}
