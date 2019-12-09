import { of as observableOf,  Observable } from 'rxjs';

import { map, filter } from 'rxjs/operators';
import { Component, OnInit } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { MatSnackBar } from '@angular/material/snack-bar';

import { LayoutFacadeService } from 'app/entities/layout/layout.facade';
import { Rule, ServiceActionType } from './rule';
import { SortDirection } from '../../types/types';
import { RulesService } from '../../services/rules/rules.service';
import { TelemetryService } from '../../services/telemetry/telemetry.service';
import {
  DeleteNotificationDialogComponent
} from 'app/page-components/delete-notification-dialog/delete-notification-dialog.component';

export interface FieldDirection {
  node_name: SortDirection;
  alert_type: SortDirection;
}

@Component({
  selector: 'app-notifications',
  templateUrl: './notifications.component.html',
  styleUrls: ['./notifications.component.scss']
})
export class NotificationsComponent implements OnInit {
  rules$: Observable<Rule[]> = observableOf([]);
  errorLoading = false;
  currentPage = 1;
  pageSize = 10;
  sortField: string;
  sortDir: FieldDirection;
  permissionDenied = false;
  // This is exposed here to allow the component HTML access to ServiceActionType
  serviceActionType = ServiceActionType;

  constructor(
    private layoutFacade: LayoutFacadeService,
    private service: RulesService,
    public dialog: MatDialog,
    public snackBar: MatSnackBar,
    private telemetryService: TelemetryService
  ) { }

  ngOnInit() {
    this.layoutFacade.showSidebar('settings');
    this.rules$ = this.service.fetchRules();
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
    this.toggleSort('node_name');
  }

  toggleSort(field: string) {
    if (field === this.sortField) {
      // when sorting is inverted for the currently sorted column
      this.sortDir[field] = this.sortDir[field] === 'asc' ? 'desc' : 'asc';
    } else {
      // when sorting a different column than the currently sorted one
      this.resetSortDir();
    }
    this.sortField = field;
    this.updateSort(field, this.sortDir[field]);
  }
  sortIcon(field: string): string {
    if (field === this.sortField) {
      return 'sort-' + this.sortDir[field];
    } else {
      return 'sort-asc';
    }
  }


  deleteRule(rule: Rule) {
    const dialogRef2 = this.dialog.open(DeleteNotificationDialogComponent);
    dialogRef2.afterClosed().pipe(
      filter((result: any) => result === 'delete' ))
      .subscribe(_result => {
        this.service.deleteRule(rule).subscribe(_res => {
          this.snackBarMessage(`Rule '${rule.name}' was deleted.`);
          this.refreshRules();
        }, err => {
          const body = err;
          this.snackBarMessage(`Could not delete rule '${rule.name}': ${body}`);
        });
      });
  }

  snackBarMessage(message) {
    this.snackBar.open(message, '', { duration: 6000 } );
  }

  private updateSort(field: string, direction: string) {
    this.rules$ = this.rules$.pipe(map(rules => {
      let sortedRules: Rule[] = [];
      if (field === 'node_name') {
        sortedRules = rules.sort((r1: Rule, r2: Rule) => {
          return r1.name.localeCompare(r2.name);
        });
      } else if (field === 'alert_type') {
        sortedRules = rules.sort((r1: Rule, r2: Rule) => {
          return r1.AlertTypeLabels[r1.ruleType].localeCompare(r2.AlertTypeLabels[r2.ruleType]);
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
      node_name: 'asc',
      alert_type: 'asc'
    };
  }

  private sendCountToTelemetry(rules: Rule[]) {
    const ruleCount = rules.reduce(
      (acc, rule: Rule) => {
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

  // TODO - this was common in all three uses, but I'm not sure this is the best
  // way to do it - do we really need to refresh after the server confirms the action
  // successful? Seem it should be possible to update the local model with the
  // changes and have that trigger view updates?
  refreshRules() {
    this.rules$ = this.service.fetchRules();
    this.rules$.subscribe(rules => {
      this.sendCountToTelemetry(rules);
      this.updateSort(this.sortField, this.sortDir[this.sortField]);
    });
  }

}
