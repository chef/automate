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

enum WebhookIntegrationTypes {
  SERVICENOW = 'ServiceNow',
  SPLUNK = 'Splunk',
  ELK_KIBANA = 'ELK/Kibana',
  CUSTOM = 'Custom'
}

enum StorageIntegrationTypes {
  MINIO = 'Minio',
  AMAZON_S3 = 'Amazon S3'
}

enum AuthTypes {
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

  private saveInProgress = false;
  private testInProgress = false;
  private testSuccess: boolean = null;
  private testError: boolean = null;

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

  set saveDone(done: boolean) {
    this.saveInProgress = done;
  }

  set testSuccessSetter(val: boolean) {
    this.dismissNotification();
    this.testSuccess = val;
  }

  set testErrorSetter(val: boolean) {
    this.dismissNotification();
    this.testError = val;
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

  get testDoneGetter() {
    return this.testInProgress;
  }

  public closeCreateSlider() {
    this.toggleSlide();
    this.createForm.reset();
    // this.integrationSelected = false;
  }

  public toggleSlide() {
    this.isSlideOpen = !this.isSlideOpen;
  }

  public slidePanel() {
    this.isSlideOpen = true;
    this.createForm.reset();
    this.integrationSelected = false;
  }

  public selectIntegration(integration: string) {
    this.showSelect = false;
    if (integration === WebhookIntegrationTypes.SERVICENOW) {
      this.createForm.reset();
      this.authSelected = AuthTypes.USERNAMEANDPASSWORD;
      this.createForm.controls['tokenType'].setValue('Bearer');
      this.integrationSelected = true;
      this.integTitle = integration;
      setTimeout(() => {
        this.showSelect = true;
        this.name.nativeElement.focus();
      });
    } else if (integration === WebhookIntegrationTypes.SPLUNK) {
      this.createForm.reset();
      this.authSelected = AuthTypes.ACCESSTOKEN;
      this.createForm.controls['tokenType'].setValue('Splunk');
      this.integrationSelected = true;
      this.integTitle = integration;
      setTimeout(() => {
        this.showSelect = true;
        this.name.nativeElement.focus();
      });
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

  public testConnection() {
    this.testInProgress = true;
    this.testDestinationEvent.emit({
      name: this.integTitle,
      auth: this.authSelected
    });
  }

  public validateForm() {
    if (this.authSelected === AuthTypes.ACCESSTOKEN) {
      if (this.createForm.get('name').valid &&
        this.createForm.get('url').valid &&
        this.createForm.get('tokenType').valid &&
        this.createForm.get('token').valid) {
        return true;
      }
    } else if (this.authSelected === AuthTypes.USERNAMEANDPASSWORD) {
      if (this.createForm.get('name').valid &&
        this.createForm.get('url').valid &&
        this.createForm.get('username').valid &&
        this.createForm.get('password').valid) {
        return true;
      }
    }
    return false;
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
  }

}
