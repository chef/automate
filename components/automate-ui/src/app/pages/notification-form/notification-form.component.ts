import { Component, OnInit } from '@angular/core';
import { Rule, ServiceActionType } from 'app/pages/notifications/rule';
import { RulesService } from 'app/services/rules/rules.service';

import { ActivatedRoute } from '@angular/router';

import { FeatureFlagsService } from '../../services/feature-flags/feature-flags.service';

enum UrlTestState {
  Inactive,
  Loading,
  Success,
  Failure
}

enum SaveState {
  Inactive,
  Success,
  Failure
}

class Model {
  constructor(
    public rule: Rule,
    public targetUsername: string,
    public targetPassword: string
  ) {
  }
}

type Modal = 'save' | 'url';

@Component({
  selector: 'app-notification-form',
  templateUrl: './notification-form.component.html',
  styleUrls: ['./notification-form.component.scss']
})
export class NotificationFormComponent implements OnInit {
  model: Model;
  urlState = UrlTestState; // expose enum to template
  saveState = SaveState; // expose enum to template
  hookStatus = UrlTestState.Inactive;
  saveStatus = SaveState.Inactive;
  alertOptions: Array<object>;
  notificationId: string;
  isEditRule: boolean;
  showLoading = false;
  targetType: string;
  urlStatusModalVisible = false;
  saveStatusModalVisible = false;
  SLACK = ServiceActionType.SLACK;
  WEBHOOK = ServiceActionType.WEBHOOK;
  SERVICENOW = ServiceActionType.SERVICENOW;
  showAsset: boolean;

  constructor(
    private rulesService: RulesService,
    private route: ActivatedRoute,
    private featureFlags: FeatureFlagsService
  ) {
    this.model = new Model(new Rule('', '', null, '', null, ''), '', '');
    this.notificationId = this.route.snapshot.params['id'];
    this.isEditRule = this.notificationId ? true : false;
    this.showLoading = this.isEditRule;
    this.showAsset = this.featureFlags.getFeatureStatus('servicenow_cmdb');

    if (this.isEditRule) {
      this.rulesService.fetchRule(this.notificationId)
        .subscribe( rule => {
          this.showLoading = false;
          this.targetType = rule.targetType;
          return this.model =
            new Model(new Rule(rule.id, rule.name, rule.ruleType,
              rule.targetUrl, rule.targetType, rule.targetSecretId), '', '');
        });
    }
  }

  ngOnInit() {
    const alertKeys = this.getAlertTypeKeys();
    this.alertOptions = alertKeys.map(key => {
      const opt = { value: key, viewValue: key };
      return opt;
    });
  }

  getAlertTypeKeys() {
    const alertKeys = this.model.rule.getAlertTypeKeys();
    if (!this.showAsset) {
      const index = alertKeys.indexOf('Assets');
      if (index > -1) {
        alertKeys.splice(index, 1);
      }
    }
    return alertKeys;
  }

  onSelectionChange(name) {
    this.targetType = name;
    this.model.rule.targetType = name;
  }

  targetTypeChosen() {
    return typeof this.targetType !== 'undefined';
  }

  getTargetTypeChecked(rule: Rule, type: string) {
    if (!this.isEditRule) {
      return false;
    }

    if (rule.targetType === type) {
      return true;
    }
  }

  setFailureType(event) {
    this.model.rule.ruleType = event.target.value;
  }

  private revealUrlStatus(status: UrlTestState) {
    this.hookStatus = status;
    this.openModal('url');
  }

  private revealSaveStatus(status: SaveState) {
    this.saveStatus = status;
    this.openModal('save');
  }

  testHook($event: Event) {
    $event.preventDefault();
    this.hookStatus = UrlTestState.Loading;
    if (this.model.targetUsername) {
      this.rulesService.testHookWithUsernamePassword(this.model.rule.targetUrl,
        this.model.targetUsername, this.model.targetPassword).subscribe(
          () => this.revealUrlStatus(UrlTestState.Success),
          () => this.revealUrlStatus(UrlTestState.Failure)
        );
    } else if (this.model.rule.targetSecretId) {
      this.rulesService.testHookWithSecretId(this.model.rule.targetUrl,
        this.model.rule.targetSecretId)
        .subscribe(
          () => this.revealUrlStatus(UrlTestState.Success),
          () => this.revealUrlStatus(UrlTestState.Failure)
        );
    } else {
      this.rulesService.testHookWithNoCreds(this.model.rule.targetUrl)
        .subscribe(
          () => this.revealUrlStatus(UrlTestState.Success),
          () => this.revealUrlStatus(UrlTestState.Failure)
        );
    }
  }

  onSubmit() {
    // Only store a username and password for ServiceNow type rules.
    if (this.model.rule.targetType !== ServiceActionType.SERVICENOW) {
      this.model.targetUsername = '';
      this.model.targetPassword = '';
    }
    if (this.isEditRule) {
      this.rulesService.editRule(this.notificationId, this.model.rule, this.model.targetUsername,
        this.model.targetPassword).subscribe(
          () => this.revealSaveStatus(SaveState.Success),
          () => this.revealSaveStatus(SaveState.Failure)
      );
    } else {
      this.rulesService.createRule(this.model.rule, this.model.targetUsername,
        this.model.targetPassword).subscribe(
          () => this.revealSaveStatus(SaveState.Success),
          () => this.revealSaveStatus(SaveState.Failure)
      );
    }
  }

  public openModal(type: Modal): void {
    switch (type) {
      case 'save':
        this.saveStatusModalVisible = true;
        return;
      case 'url':
        this.urlStatusModalVisible = true;
        return;
      default:
        return;
    }
  }

  public closeModal(type: Modal): void {
    switch (type) {
      case 'save':
        this.saveStatusModalVisible = false;
        return;
      case 'url':
        this.urlStatusModalVisible = false;
        return;
      default:
        return;
    }
  }
}
