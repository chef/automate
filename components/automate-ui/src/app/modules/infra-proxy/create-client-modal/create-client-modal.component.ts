import { Component, EventEmitter, Input, OnDestroy, OnInit } from '@angular/core';
import { IdMapper } from 'app/helpers/auth/id-mapper';
import { Store } from '@ngrx/store';
import { FormBuilder,  Validators, FormGroup } from '@angular/forms';
import { takeUntil } from 'rxjs/operators';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Regex } from 'app/helpers/auth/regex';
import { Subject, combineLatest } from 'rxjs';
import { CreateClient, GetClients } from 'app/entities/clients/client.action';
import { saveError, createClient } from 'app/entities/clients/client.selectors';
import { isNil } from 'lodash/fp';
import { saveAs } from 'file-saver';
import { Utilities } from 'app/helpers/utilities/utilities';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

@Component({
  selector: 'app-create-client-modal',
  templateUrl: './create-client-modal.component.html',
  styleUrls: ['./create-client-modal.component.scss']
})
export class CreateClientModalComponent implements OnInit, OnDestroy {
  @Input() openEvent: EventEmitter<boolean>;
  @Input() serverId: string;
  @Input() orgId: string;
  @Input() currentPage: number;

  public checkedValidator = false;
  public createdClient: string;
  public creating = false;
  public created = false;
  public conflictError = false;
  public createForm: FormGroup;
  public client_key: any;
  public conflictErrorEvent = new EventEmitter<boolean>();
  public close = new EventEmitter();
  public error: string;
  public privateKey: string;
  public org: string;
  public server: string;
  public validator = false;
  public visible = false;
  public per_page = 9;
  private isDestroyed = new Subject<boolean>();

  constructor(
    private fb: FormBuilder,
    private store: Store<NgrxStateAtom>,
    private telemetryService: TelemetryService
  ) {
    this.createForm = this.fb.group({
      name: ['', [Validators.required,
        Validators.pattern(Regex.patterns.NO_WILDCARD_ALLOW_HYPHEN)]],
      validator: ['']
    });
  }

  ngOnInit() {
    this.openEvent.pipe(takeUntil(this.isDestroyed))
    .subscribe(() => {
    this.created = false;
    this.conflictError = false;
    this.visible = true;
    this.server = this.serverId;
    this.org = this.orgId;
    this.error = '';
    this.privateKey = '';
    this.checkedValidator = false;
  });

  combineLatest([
    this.store.select(saveError),
    this.store.select(createClient)
  ]).pipe(
    takeUntil(this.isDestroyed))
    .subscribe(([ errorSt, createState]) => {
      if ( !isNil(errorSt) ) {
        this.created = false;
        this.creating = false;
        this.error = errorSt?.message;
      } else {
        this.creating = false;
        this.created = true;
        this.createdClient = createState?.name;
        this.client_key = createState?.client_key;
        this.privateKey = this.client_key?.private_key;
      }
    });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  handleNameInput(event: KeyboardEvent): void {
    if (!Utilities.isNavigationKey(event)) {
      this.conflictError = false;
      this.error = '';
      this.createForm.controls.name.setValue(
        IdMapper.transform(this.createForm.controls.name.value.trim()));
    }
  }

  public handleInput(event: KeyboardEvent): void {
    if (Utilities.isNavigationKey(event)) {
      return;
    }
    this.conflictError = false;
  }

  updateValidatorCheckbox(event: boolean): void {
    this.checkedValidator = event;
    this.createForm.controls.validator.setValue(this.checkedValidator);
  }

  closeCreateModal(): void {
    this.resetCreateModal();
    this.visible = false;
    const payload = {
      clientName: '',
      server_id: this.serverId,
      org_id: this.orgId,
      page: this.currentPage,
      per_page: this.per_page
    };
    this.store.dispatch(new GetClients(payload));
  }

  private resetCreateModal(): void {
    this.creating = false;
    this.created = false;
    this.error = '';
    this.privateKey = '';
    this.createForm.reset();
    this.checkedValidator = false;
    this.conflictErrorEvent.emit(false);
  }

  createClient(): void {
    this.creating = true;
    const client = {
      name: this.createForm.controls['name'].value.trim(),
      validator: this.createForm.controls['validator'].value || this.checkedValidator,
      server_id: this.serverId,
      org_id: this.orgId,
      create_key : true
    };
    this.store.dispatch(new CreateClient(client));
    this.telemetryService.track('InfraServer_Clients_Create');
  }

  downloadKey() {
    const template = `
    Private RSA Key

    ${this.privateKey}
    `;

    const blob = new Blob([template], { type: 'text/plain;charset=utf-8' });
    saveAs(blob, this.createdClient + '.pem');
    this.telemetryService.track('InfraServer_Clients_Download_PrivateKey');
  }

}
