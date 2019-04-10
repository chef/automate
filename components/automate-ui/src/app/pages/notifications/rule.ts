export type RuleType = 'CCRFailure' | 'ComplianceFailure' | 'Assets';

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
  targetType: ServiceActionType; // Slack or Custom
}


export class Rule implements RuleInterface {

  AlertTypeLabels = {
    CCRFailure: 'Chef client run failures',
    ComplianceFailure: 'InSpec scan failures',
    Assets: 'Assets'
  };

  constructor(
    public id: string,
    public name: string,
    public ruleType: RuleType,
    public targetUrl: string,
    public targetType: ServiceActionType,
    public targetSecretId: string
  ) {
  }

  public static fromResponse(rule: Object): Rule {
    const ruleTargetInfo = this.extractTargetInfo(rule);
    return new Rule(rule['id'], rule['name'], rule['event'],
      ruleTargetInfo.targetUrl, ruleTargetInfo.targetType, ruleTargetInfo.targetSecretId);
  }

  private static extractTargetInfo(rule: Object): {targetType: ServiceActionType,
      targetUrl: string, targetSecretId: string} {
    const slack_action = rule[ServiceActionType.SLACK];
    const webhook_action = rule[ServiceActionType.WEBHOOK];
    const servicenow_action = rule[ServiceActionType.SERVICENOW];

    if (slack_action !== null && slack_action !== undefined) {
      return {targetType: ServiceActionType.SLACK, targetUrl: slack_action['url'],
      targetSecretId: ''};
    } else if (webhook_action !== null && webhook_action !== undefined) {
      return {targetType: ServiceActionType.WEBHOOK, targetUrl: webhook_action['url'],
      targetSecretId: ''};
    } else if (servicenow_action !== null && servicenow_action !== undefined) {
        return {targetType: ServiceActionType.SERVICENOW, targetUrl: servicenow_action['url'],
        targetSecretId: servicenow_action['secret_id']};
    } else {
      console.error(`Could not find action in rule ${JSON.stringify(rule)}`);
      throw new TypeError('Unable to extract action type');
    }
  }

  public getAlertTypeKeys(): string[] {
    return Object.keys(this.AlertTypeLabels);
  }

  public toRequest(): Object {
    let target: any = {'url': this.targetUrl};
    if (this.targetSecretId !== undefined && this.targetSecretId !== '' ) {
      target = {
        'url': this.targetUrl,
        'secret_id': this.targetSecretId
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
