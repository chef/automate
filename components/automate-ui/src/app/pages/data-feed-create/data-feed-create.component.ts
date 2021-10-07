import {
  Component,
  Input,
  EventEmitter,
  Output,
  HostBinding,
  ViewChild,
  ElementRef
} from '@angular/core';
import { FormGroup } from '@angular/forms';
import { Revision } from 'app/entities/revisions/revision.model';
import { Regex } from 'app/helpers/auth/regex';
import { regions } from 'app/entities/destinations/destination.model';

export enum WebhookIntegrationTypes {
  SERVICENOW = 'ServiceNow',
  SPLUNK = 'Splunk',
  ELK_KIBANA = 'ELK',
  CUSTOM = 'Custom'
}

export enum StorageIntegrationTypes {
  MINIO = 'Minio',
  AMAZON_S3 = 'S3'
}

export enum AuthTypes {
  ACCESSTOKEN = 'Access Token',
  USERNAMEANDPASSWORD = 'Username and Password'
}

export enum IntegrationTypes {
  WEBHOOK = 'Webhook',
  STORAGE = 'Storage'
}

@Component({
  selector: 'app-data-feed-create',
  templateUrl: './data-feed-create.component.html',
  styleUrls: ['./data-feed-create.component.scss']
})
export class DataFeedCreateComponent {
  @Input() createForm: FormGroup;
  @Output() saveDestinationEvent = new EventEmitter<any>();
  @Output() testDestinationEvent = new EventEmitter<any>();
  @Output() checkEvent = new EventEmitter<any>();

  public revisions: Revision[] = [];
  public revisionsListLoading = true;
  @HostBinding('class.active') isSlideOpen = false;

  @ViewChild('name', { static: true }) name: ElementRef;

  public integTitle: string;
  public integrationSelected = false;
  public tokenToggle = true;
  public hideNotification = true;
  public authSelected: string = AuthTypes.ACCESSTOKEN;
  public showSelect = false;
  public flagHeaders = true;
  public validHeadersValue = false;

  private saveInProgress = false;
  private testInProgress = false;
  public headerChecked = false;
  public notificationShow = false;
  public notificationMessage = '';
  public notificationType = 'error';
  public dropDownVal = null;
  public awsRegions = regions;

  public integrations = {
    webhook: [
      {name: WebhookIntegrationTypes.SERVICENOW, asset: 'servicenow'},
      {name: WebhookIntegrationTypes.SPLUNK, asset: 'splunk'},
      {name: WebhookIntegrationTypes.ELK_KIBANA, asset: 'elk'},
      {name: WebhookIntegrationTypes.CUSTOM, asset: 'custom'}
    ],
    storage: [
      {name: StorageIntegrationTypes.MINIO, asset: 'minio'},
      {name: StorageIntegrationTypes.AMAZON_S3, asset: 's3'}
    ]
  };

  public showFields = {
    name: false,
    endpoint: false,
    region: false,
    url: false,
    authSelector: false,
    tokenType: false,
    token: false,
    username: false,
    password: false,
    headers: false,
    bucketName: false,
    accessKey: false,
    secretKey: false,
    useHeaders: false,
    customToken: false
  };

  set saveDone(done: boolean) {
    this.saveInProgress = done;
  }

  set testSuccessSetter(val: boolean) {
    this.dismissNotification();
    const message = 'Notification test connected successfully!';
    this.showNotification(val, message, 'info');
  }

  set testErrorSetter(val: boolean) {
    let errorString: string;
    if (this.integTitle === StorageIntegrationTypes.MINIO) {
      errorString =
        'Unable to connect: check endpoint, bucket name, access key and secret key.';
    } else if (this.authSelected === AuthTypes.USERNAMEANDPASSWORD) {
      errorString = 'Unable to connect: check URL, username and password.';
    } else if (this.authSelected === AuthTypes.ACCESSTOKEN) {
      errorString = 'Unable to connect: check Token Type (Prefix) and token.';
    }
    this.dismissNotification();
    this.showNotification(val, errorString, 'error');
  }

  public showNotification(show: boolean, message: string, type: string) {
    setTimeout(() => {
      this.notificationShow = show;
    });
    this.notificationType = type;
    this.notificationMessage = message;
  }

  set conflictErrorSetter(val: string) {
    this.saveDone = false;
    this.dismissNotification();
    this.showNotification(true, val, 'error');
  }

  set testDoneSetter(done: boolean) {
    this.testInProgress = done;
  }

  get saveDoneGetter() {
    return this.saveInProgress;
  }

  get testDoneGetter() {
    return this.testInProgress;
  }

  public closeCreateSlider() {
    this.toggleSlide();
  }

  public toggleSlide() {
    this.isSlideOpen = !this.isSlideOpen;
  }

  public slidePanel() {
    this.isSlideOpen = true;
    this.integrationSelected = false;
  }

  showFieldWebhook() {
    Object.keys(this.showFields).forEach((v) => (this.showFields[v] = false));
    this.showFields = {
      ...this.showFields,
      ...{
        name: true,
        url: true,
        authSelector: true,
        tokenType: true,
        token: true,
        username: true,
        password: true
      }
    };
  }

  updateHeaderCheckbox(event: boolean): void {
    this.checkEvent.emit(event);
    this.headerChecked = event;
  }

  showFieldStorage() {
    Object.keys(this.showFields).forEach((v) => (this.showFields[v] = false));
    this.showFields = {
      ...this.showFields,
      ...{
        name: true,
        endpoint: true,
        region: true,
        bucketName: true,
        accessKey: true,
        secretKey: true
      }
    };
  }

  public selectIntegration(integration: string) {
    this.showSelect = false;
    this.showFields.headers = false;
    this.integTitle = integration;
    this.createForm.reset();
    this.headerChecked = false;
    this.flagHeaders = true;
    setTimeout(() => {
      this.showSelect = true;
      this.name.nativeElement.focus();
    });

    switch (integration) {
      case WebhookIntegrationTypes.SERVICENOW: {
        this.showFieldWebhook();
        this.authSelected = AuthTypes.USERNAMEANDPASSWORD;
        this.createForm.controls['tokenType'].setValue('Bearer');
        this.integrationSelected = true;
        break;
      }
      case WebhookIntegrationTypes.SPLUNK: {
        this.showFieldWebhook();
        this.authSelected = AuthTypes.ACCESSTOKEN;
        this.createForm.controls['tokenType'].setValue('Splunk');
        this.integrationSelected = true;
        break;
      }
      case WebhookIntegrationTypes.CUSTOM: {
        this.showFieldWebhook();
        this.showFields.customToken = true;
        this.showFields.useHeaders = true;
        this.authSelected = AuthTypes.ACCESSTOKEN;
        this.createForm.controls['tokenType'].setValue('');
        this.integrationSelected = true;
        break;
      }
      case WebhookIntegrationTypes.ELK_KIBANA: {
        this.showFieldWebhook();
        this.authSelected = AuthTypes.USERNAMEANDPASSWORD;
        this.createForm.controls['tokenType'].setValue('Bearer');
        this.integrationSelected = true;
        break;
      }
      case StorageIntegrationTypes.MINIO: {
        this.showFieldStorage();
        this.showFields.region = false;
        this.integrationSelected = true;
        break;
      }
      case StorageIntegrationTypes.AMAZON_S3: {
        this.dropDownVal = 'us-east-2';
        this.showFieldStorage();
        this.showFields.endpoint = false;
        this.integrationSelected = true;
        break;
      }
    }
  }

  public returnToMenu() {
    this.integrationSelected = false;
  }

  public toggleTokenType() {
    this.tokenToggle = !this.tokenToggle;
  }

  public selectChangeHandlers(type: string) {
    this.authSelected = type;
  }

  public dropDownChangeHandlers(val: string) {
    this.dropDownVal = val;
  }

  public testConnection() {
    this.testInProgress = true;
    this.testDestinationEvent.emit({
      name: this.integTitle,
      auth: this.authSelected,
      region: this.dropDownVal
    });
  }

  public validateForm() {
    switch (this.integTitle) {
      case WebhookIntegrationTypes.SERVICENOW:
      case WebhookIntegrationTypes.SPLUNK:
      case WebhookIntegrationTypes.ELK_KIBANA:
      case WebhookIntegrationTypes.CUSTOM: {
        // handling access token and user pass auth
        // for servicenow, splunk, elk and custom
        switch (this.authSelected) {
          case AuthTypes.ACCESSTOKEN: {
          if (this.createForm.get('name').valid && this.createForm.get('url').valid &&
            this.createForm.get('tokenType').valid && this.createForm.get('token').valid) {
              if (this.integTitle === WebhookIntegrationTypes.CUSTOM && this.headerChecked &&
                this.validHeadersValue && this.flagHeaders) {
                return true;
            } else if (this.integTitle === WebhookIntegrationTypes.CUSTOM &&
              !this.headerChecked && this.flagHeaders) {
              return true;
            } else if (this.integTitle !== WebhookIntegrationTypes.CUSTOM) {
            return true;
            }
            }
            break;
          }

          case AuthTypes.USERNAMEANDPASSWORD: {

            if (this.createForm.get('name').valid && this.createForm.get('url').valid &&
              this.createForm.get('username').valid && this.createForm.get('password').valid) {
              if (this.integTitle === WebhookIntegrationTypes.CUSTOM && this.headerChecked &&
                this.validHeadersValue && this.flagHeaders) {
                return true;
              } else if (this.integTitle === WebhookIntegrationTypes.CUSTOM &&
                !this.headerChecked && this.flagHeaders) {
                return true;
              } else if (this.integTitle !== WebhookIntegrationTypes.CUSTOM) {
                return true;
              }
            }
          }
        }
        break;
      }
      case StorageIntegrationTypes.MINIO: {
        // handling minio
        if (
          this.createForm.get('name').valid &&
          this.createForm.get('endpoint').valid &&
          this.createForm.get('bucketName').valid &&
          this.createForm.get('accessKey').valid &&
          this.createForm.get('secretKey').valid
        ) {
          return true;
        }
        break;
      }
      case StorageIntegrationTypes.AMAZON_S3: {
        if (
          this.createForm.get('name').valid &&
          this.createForm.get('bucketName').valid &&
          this.createForm.get('accessKey').valid &&
          this.createForm.get('secretKey').valid
        ) {
          return true;
        }
      }
    }
    return false;
  }

  public saveDestination() {
    this.saveInProgress = true;
    this.saveDestinationEvent.emit({
      name: this.integTitle,
      auth: this.authSelected,
      region: this.dropDownVal
    });
  }

  public dismissNotification() {
    this.notificationShow = false;
  }

  public showAuthDropdown() {
    return this.showFields.authSelector && this.showSelect;
  }

  public showTokenInput(field: string) {
    return (this.showFields[field] && this.authSelected === AuthTypes.ACCESSTOKEN);
  }

  public showUserPassInput(field: string) {
    return (this.showFields[field] && this.authSelected === AuthTypes.USERNAMEANDPASSWORD);
  }

  public validateHeaders(customHeaders: string): void {
    const headersVal = customHeaders.split('\n');
    for (const values in headersVal) {
      if (this.headerChecked && headersVal[values]) {
        this.flagHeaders = Regex.patterns.VALID_HEADER.test(headersVal[values]);
        this.validHeadersValue = this.flagHeaders;
        if (this.flagHeaders === false) {
          break;
        }
      } else if (headersVal[values] === '') {
        this.flagHeaders = true;
      }
    }
  }
}
