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

export enum WebhookIntegrationTypes {
  SERVICENOW = 'ServiceNow',
  SPLUNK = 'Splunk',
  ELK_KIBANA = 'ELK/Kibana',
  CUSTOM = 'Custom'
}

export enum StorageIntegrationTypes {
  MINIO = 'Minio',
  AMAZON_S3 = 'Amazon S3'
}

export enum AuthTypes {
  ACCESSTOKEN = 'Access Token',
  USERNAMEANDPASSWORD = 'Username and Password'
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
  public errorString: string;

  private saveInProgress = false;
  private testInProgress = false;
  private testSuccess: boolean = null;
  private testError: boolean = null;
  private conflictError: string = null;

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
    secretKey: false
  };

  set saveDone(done: boolean) {
    this.saveInProgress = done;
  }

  set testSuccessSetter(val: boolean) {
    this.dismissNotification();
    this.testSuccess = val;
  }

  set testErrorSetter(val: boolean) {
    if (this.integTitle === 'Minio') {
      this.errorString = 'Unable to connect: check endpoint, bucket name, access key and secret key.';
    } else if (this.authSelected === 'Username and Password') {
      this.errorString = 'Unable to connect: check URL, username and password.';
    } else if (this.authSelected === 'Access Token') {
      this.errorString = 'Unable to connect: check Token Type (Prefix) and token.';
    }
    this.dismissNotification();
    this.testError = val;
  }

  set conflictErrorSetter(val: string) {
    this.saveDone = false;
    this.dismissNotification();
    this.conflictError = val;
  }

  set testDoneSetter(done: boolean) {
    this.testInProgress = done;
  }

  get saveDoneGetter() {
    return this.saveInProgress;
  }

  get testSuccessGetter() {
    return this.testSuccess;
  }

  get testErrorGetter() {
    return this.testError;
  }

  get conflictErrorGetter() {
    return this.conflictError;
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
    Object.keys(this.showFields).forEach(v => this.showFields[v] = false);
    this.showFields = {...this.showFields, ...{
      name: true,
      url: true,
      authSelector: true,
      tokenType: true,
      token: true,
      username: true,
      password: true
    }};
  }

  showFieldStorage() {
    Object.keys(this.showFields).forEach(v => this.showFields[v] = false);
    this.showFields = {...this.showFields, ...{
      name: true,
      endpoint: true,
      region: true,
      bucketName: true,
      accessKey: true,
      secretKey: true
    }};
  }

  public selectIntegration(integration: string) {
    this.showSelect = false;
    this.showFields.headers = false;
    this.integTitle = integration;
    this.createForm.reset();
    setTimeout(() => {
      this.showSelect = true;
      this.name.nativeElement.focus();
    });

    if (integration === WebhookIntegrationTypes.SERVICENOW) {
      this.showFieldWebhook();
      this.authSelected = AuthTypes.USERNAMEANDPASSWORD;
      this.createForm.controls['tokenType'].setValue('Bearer');
      this.integrationSelected = true;

    } else if (integration === WebhookIntegrationTypes.SPLUNK) {
      this.showFieldWebhook();
      this.authSelected = AuthTypes.ACCESSTOKEN;
      this.createForm.controls['tokenType'].setValue('Splunk');
      this.integrationSelected = true;

    } else if (integration === StorageIntegrationTypes.MINIO) {
      this.showFieldStorage();
      this.showFields.region = false;
      this.integrationSelected = true;

    }

    // else if (integration === WebhookIntegrationTypes.ELK_KIBANA) {
    //   this.showFieldWebhook();
    //   this.authSelected = AuthTypes.ACCESSTOKEN;
    //   this.createForm.controls['tokenType'].setValue('Bearer');

    // }
    // else if (integration === WebhookIntegrationTypes.CUSTOM) {
    //   this.showFieldWebhook();
    //   this.showFields.headers = true;
    //   this.authSelected = AuthTypes.ACCESSTOKEN;
    //   this.createForm.controls['tokenType'].setValue('Bearer');

    // }
    // else if (integration === StorageIntegrationTypes.AMAZON_S3) {
    //   this.showFieldStorage();
    //   this.showFields.endpoint = false;
    //   this.createForm.reset();
    // }
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

  public testConnection() {
    this.testInProgress = true;
    this.testDestinationEvent.emit({
      name: this.integTitle,
      auth: this.authSelected
    });
  }

  public validateForm() {
    if (this.integTitle === WebhookIntegrationTypes.SERVICENOW ||
      this.integTitle === WebhookIntegrationTypes.SPLUNK ||
      this.integTitle === WebhookIntegrationTypes.ELK_KIBANA) {
      if (this.authSelected === AuthTypes.ACCESSTOKEN) {
        if (this.createForm.get('name').valid && this.createForm.get('url').valid &&
          this.createForm.get('tokenType').valid && this.createForm.get('token').valid) {
          return true;
        }
      } else if (this.authSelected === AuthTypes.USERNAMEANDPASSWORD) {
        if (this.createForm.get('name').valid && this.createForm.get('url').valid &&
          this.createForm.get('username').valid && this.createForm.get('password').valid) {
          return true;
        }
      }
    } else if (this.integTitle === WebhookIntegrationTypes.CUSTOM) {

    } else if (this.integTitle === StorageIntegrationTypes.MINIO) {
        if (this.createForm.get('name').valid && this.createForm.get('endpoint').valid &&
          this.createForm.get('bucketName').valid && this.createForm.get('accessKey').valid &&
          this.createForm.get('secretKey').valid) {
          return true;
        }
    }
    return false;

    // else if (this.integTitle === StorageIntegrationTypes.AMAZON_S3) {
    //     if (this.createForm.get('name').valid && this.createForm.get('region').valid &&
    //       this.createForm.get('bucketName').valid && this.createForm.get('accessKey').valid &&
    //       this.createForm.get('secretKey').valid) {
    //       return true;
    //     }
    // }
  }

  public saveDestination() {
    this.saveInProgress = true;
    this.saveDestinationEvent.emit({
      name: this.integTitle,
      auth: this.authSelected
    });
  }

  public dismissNotification() {
    this.testSuccess = false;
    this.testError = false;
    this.conflictError = null;
  }

}
