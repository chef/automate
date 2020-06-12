export type RuleType = 'CCRFailure' | 'ComplianceFailure';

// The target type used by the service
export enum ServiceActionType {
  SLACK = 'SlackAlert',
  WEBHOOK = 'WebhookAlert',
  SERVICENOW = 'ServiceNowAlert'
}

export interface RuleInterface {
  id: string;
  name: string; // name of the rule
  ruleType: RuleType; // Compliance or CCR
  targetUrl: string; // URL of the webhook or slack notification
  targetType: ServiceActionType;
}


export class NotificationRule implements RuleInterface {

  AlertTypeLabels = {
    CCRFailure: 'Infra Client run failures',
    ComplianceFailure: 'InSpec compliance scan failure'
  };

  TargetTypeLabels = {
    SlackAlert: 'Slack',
    WebhookAlert: 'Webhook',
    ServiceNowAlert: 'ServiceNow'
  };

  constructor(
    public id: string,
    public name: string,
    public ruleType: RuleType,
    public targetUrl: string,
    public targetType: ServiceActionType,
    public targetSecretId: string,
    public criticalControlsOnly: boolean
  ) {
  }

  public static fromResponse(rule: Object): NotificationRule {
    const ruleTargetInfo = this.extractTargetInfo(rule);
    return new NotificationRule(rule['id'], rule['name'], rule['event'],
      ruleTargetInfo.targetUrl, ruleTargetInfo.targetType, ruleTargetInfo.targetSecretId,
      ruleTargetInfo.criticalControlsOnly);
  }

  private static extractTargetInfo(rule: Object): {targetType: ServiceActionType,
      targetUrl: string, targetSecretId: string, criticalControlsOnly: boolean} {
    const slack_action = rule[ServiceActionType.SLACK];
    const webhook_action = rule[ServiceActionType.WEBHOOK];
    const servicenow_action = rule[ServiceActionType.SERVICENOW];

    if (slack_action !== null && slack_action !== undefined) {
      return {targetType: ServiceActionType.SLACK, targetUrl: slack_action['url'],
      targetSecretId: '', criticalControlsOnly: false};
    } else if (webhook_action !== null && webhook_action !== undefined) {
      return {targetType: ServiceActionType.WEBHOOK, targetUrl: webhook_action['url'],
      targetSecretId: '', criticalControlsOnly: false};
    } else if (servicenow_action !== null && servicenow_action !== undefined) {
      return {targetType: ServiceActionType.SERVICENOW, targetUrl: servicenow_action['url'],
      targetSecretId: servicenow_action['secret_id'],
      criticalControlsOnly: servicenow_action['critical_controls_only']};
    } else {
      console.error(`Could not find action in rule ${JSON.stringify(rule)}`);
      throw new TypeError('Unable to extract action type');
    }
  }

  public getAlertTypeKeys(): string[] {
    return Object.keys(this.AlertTypeLabels);
  }

  public getTargetTypeKeys(): string[] {
    return Object.keys(this.TargetTypeLabels);
  }

  public toRequest(): Object {
    let target: any = {'url': this.targetUrl};
    if (this.targetSecretId !== undefined && this.targetSecretId !== '' ) {
      target = {
        'url': this.targetUrl,
        'secret_id': this.targetSecretId,
        'critical_controls_only': this.criticalControlsOnly
      };
    }

    return {
      'rule': {
        'name': this.name,
        'event': this.ruleType,
        [this.targetType]: target
      }
    };
  }
}
