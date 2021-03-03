import { Component, EventEmitter, Input, OnInit } from '@angular/core';
import { IdMapper } from 'app/helpers/auth/id-mapper';
import { Store } from '@ngrx/store';
import { FormBuilder,  Validators, FormGroup } from '@angular/forms';
import { takeUntil } from 'rxjs/operators';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Regex } from 'app/helpers/auth/regex';
import { Subject, combineLatest } from 'rxjs';
import { CreateClient } from 'app/entities/clients/client.action';
import {
  getAllStatus,
  saveError,
  createClient
} from 'app/entities/clients/client.selectors';
import { isNil } from 'lodash/fp';
import { saveAs } from 'file-saver';

@Component({
  selector: 'app-create-client-modal',
  templateUrl: './create-client-modal.component.html',
  styleUrls: ['./create-client-modal.component.scss']
})
export class CreateClientModalComponent implements OnInit {
  @Input() openEvent: EventEmitter<boolean>;
  @Input() serverId: string;
  @Input() orgId: string;

  public checkedValidator = false;
  public client_key;  
  public createdClient: string;
  public creating = false;
  public created = false;
  public conflictError = false;
  public createForm: FormGroup;
  public conflictErrorEvent = new EventEmitter<boolean>();
  public close = new EventEmitter();  
  public error: string;
  public privateKey;
  public org: string;
  public server: string;
  public validator = false;
  public visible = false; 
  private isDestroyed = new Subject<boolean>();

  constructor(
    private fb: FormBuilder,
    private store: Store<NgrxStateAtom>
  ) {
    this.createForm = this.fb.group({
      name: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
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
  });

  combineLatest([
    this.store.select(getAllStatus),
    this.store.select(saveError),
    this.store.select(createClient)
  ]).pipe(
    takeUntil(this.isDestroyed))
    .subscribe(([_, errorSt, createState]) => {
      if ( !isNil(createState)) {
        this.created = true;
        this.createdClient = createState?.name;
        this.client_key = createState?.client_key;
        this.privateKey = this.client_key?.private_key;
      } else {
        this.created = false;
        this.error = errorSt?.message;
      }
    });
  }

  handleNameInput(event: KeyboardEvent): void {
    if (!this.isNavigationKey(event)) {
      this.conflictError = false;
      this.error = '';
      this.createForm.controls.name.setValue(
        IdMapper.transform(this.createForm.controls.name.value.trim()));
    }
  }

  public handleInput(event: KeyboardEvent): void {
    if (this.isNavigationKey(event)) {
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
  }

  private resetCreateModal(): void {
    this.created = false;
    this.error = '';
    this.privateKey = '';
    this.createForm.reset();
    this.validator = false;
    this.conflictErrorEvent.emit(false);
  }

  createClient(): void {
    this.creating = true;
    const client = {
      name: this.createForm.controls['name'].value.trim(),
      validator: this.createForm.controls['validator'].value || this.validator,
      server_id: this.serverId,
      org_id: this.orgId,
      create_key : true
    };
    this.store.dispatch(new CreateClient(client));
  }

  downloadKey() {
    const template = `
    Private RSA Key

    ${this.privateKey}
    `;

    const blob = new Blob([template], { type: 'text/plain;charset=utf-8' });
    saveAs(blob, this.createdClient + '.pem');
  }

  private isNavigationKey(event: KeyboardEvent): boolean {
    return event.key === 'Shift' || event.key === 'Tab';
  }
}
