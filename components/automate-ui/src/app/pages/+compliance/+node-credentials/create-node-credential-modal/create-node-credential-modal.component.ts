import {
  Component, EventEmitter, Input, OnInit, OnDestroy
} from '@angular/core';
import { isNil } from 'lodash/fp';
import { FormGroup, FormBuilder, Validators } from '@angular/forms';
import { Store } from '@ngrx/store';
import { Subject, combineLatest } from 'rxjs';
import { takeUntil, filter } from 'rxjs/operators';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { IdMapper } from 'app/helpers/auth/id-mapper';
import { EntityStatus } from 'app/entities/entities';
import { Utilities } from 'app/helpers/utilities/utilities';
import {
  saveError,
  saveStatus
} from 'app/entities/node-credentials/node-credential.selectors';
import { SaveNodeCredential } from 'app/entities/node-credentials/node-credential.model';
import { CreateNodeCredential, NodeCredentialsSearch } from 'app/entities/node-credentials/node-credential.actions';

@Component({
  selector: 'app-create-node-credential-modal',
  templateUrl: './create-node-credential-modal.component.html',
  styleUrls: ['./create-node-credential-modal.component.scss']
})

export class CreateNodeCredentialModalComponent implements OnInit, OnDestroy {
  @Input() openEvent: EventEmitter<boolean>;
  @Input() sortParams: object;

  private isDestroyed = new Subject<boolean>();
  public createNodeCredForm: FormGroup;
  public sshForms: FormGroup;
  public winrmForms: FormGroup;
  public sudoForms: FormGroup;
  public conflictError = false;
  public creatingUser = false;
  public visible = false;
  public modifyID = false;
  public selected = 'ssh';
  public types: string[] = ['Password', 'RSA'];
  public passwordSelected = 'Password';

  constructor(
    private store: Store<NgrxStateAtom>,
    private saveNodeCred: SaveNodeCredential,
    private fb: FormBuilder
  ) {
    this.sshForms = this.fb.group({
      username: ['', Validators.required],
      password: ['', Validators.required],
      key: ['', Validators.required]
    });
    this.winrmForms = this.fb.group({
      username: ['', Validators.required],
      password: ['', Validators.required]
    });
    this.sudoForms = this.fb.group({
      password: ['', Validators.required],
      options: ['', Validators.required]
    });
    this.createNodeCredForm = this.fb.group({
      id: [''],
      name: ['', Validators.required],
      type: ['ssh', Validators.required],
      ssh: this.sshForms,
      winrm: this.winrmForms,
      sudo: this.sudoForms
    });
  }

  ngOnInit(): void {
    this.openEvent.pipe(takeUntil(this.isDestroyed))
      .subscribe(() => {
        this.creatingUser = false;
        this.conflictError = false;
        this.selected = 'ssh';
        this.createNodeCredForm.reset();
        this.createNodeCredForm.controls['type'].setValue('ssh');
        this.visible = true;
      });
    this.store.select(saveStatus).pipe(
      filter(state => state === EntityStatus.loadingSuccess),
      takeUntil(this.isDestroyed))
      .subscribe(state => {
        this.creatingUser = false;
        if (state === EntityStatus.loadingSuccess) {
          this.store.dispatch(new NodeCredentialsSearch(this.sortParams));
          this.closeCreateModal();
        }
      });

    combineLatest([
      this.store.select(saveStatus),
      this.store.select(saveError)
    ]).pipe(
      takeUntil(this.isDestroyed),
      filter(([state, error]) => state === EntityStatus.loadingFailure && !isNil(error)))
      .subscribe(([_, error]) => {
        if (error) {
          this.conflictError = true;
        } else {
          this.creatingUser = false;
          this.store.dispatch(new NodeCredentialsSearch(this.sortParams));
          this.closeCreateModal();
        }
      });
  }

  closeCreateModal(): void {
    this.visible = false;
  }

  createUser(): void {
    this.creatingUser = true;
    const formValues = this.createNodeCredForm.value;

    const userCreateReq = this.saveNodeCred.getNodeCredentialCreate(formValues);

    this.store.dispatch(new CreateNodeCredential(userCreateReq));
  }

  selectChangeHandlers(id: string): void {
    this.passwordSelected = id;
  }

  handleNameInput(event: KeyboardEvent): void {
    if (!this.modifyID && !Utilities.isNavigationKey(event)) {
      this.conflictError = false;
      this.createNodeCredForm.controls.id.setValue(
        IdMapper.transform(this.createNodeCredForm.controls.name.value.trim()));
    }
  }

  updateFormDisplay(id: string): void {
    this.selected = id;
  }

  handleInput(event: KeyboardEvent): void {
    if (Utilities.isNavigationKey(event)) {
      return;
    }
    this.conflictError = false;
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

}
