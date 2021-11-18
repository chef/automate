import { Component, OnInit, OnDestroy } from '@angular/core';
import { Router } from '@angular/router';
import { Store, select } from '@ngrx/store';
import { FormGroup, FormBuilder, Validators } from '@angular/forms';
import { combineLatest, Subject } from 'rxjs';
import { filter, pluck, takeUntil } from 'rxjs/operators';
import { identity, isNil } from 'lodash/fp';

import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { routeParams, routeURL } from 'app/route.selectors';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { GetNodeCredential, UpdateNodeCredential } from 'app/entities/node-credentials/node-credential.actions';
import { credentialFromRoute, getStatus } from 'app/entities/node-credentials/node-credential-details.selectors';
import { updateStatus } from 'app/entities/node-credentials/node-credential.selectors';
import { NodeCredential, SaveNodeCredential, NodeObject } from 'app/entities/node-credentials/node-credential.model';
import { pending, EntityStatus, allLoaded } from 'app/entities/entities';

export type NodeCredentialTabName = 'details' | 'reset';

@Component({
  selector: 'app-node-credential-details-screen',
  templateUrl: './node-credential-details.component.html',
  styleUrls: ['./node-credential-details.component.scss']
})

export class NodeCredentialDetailsScreenComponent implements OnInit, OnDestroy {
  private isDestroyed = new Subject<boolean>();
  public nodeCredential: NodeCredential;
  public tabValue: NodeCredentialTabName = 'details';
  public types: string[] = ['Password', 'RSA'];
  public passwordSelected = 'Password';
  public updateForm: FormGroup;
  public resetForm: FormGroup;
  public sshForms: FormGroup;
  public winrmForms: FormGroup;
  public sudoForms: FormGroup;
  public id: string;
  public url: string;
  public saveSuccessful = false;
  public saveInProgress = false;
  public resetSuccessful = false;
  public resetInProgress = false;
  public isLoading = true;
  public nodeDetailsLoading = true;

  constructor(
    private store: Store<NgrxStateAtom>,
    public saveCred: SaveNodeCredential,
    private fb: FormBuilder,
    private router: Router,
    private layoutFacade: LayoutFacadeService
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

    this.updateForm = this.fb.group({
      id: [''],
      name: ['', Validators.required],
      type: ['', Validators.required],
      ssh: this.sshForms,
      winrm: this.winrmForms,
      sudo: this.sudoForms
    });

    this.resetForm = this.fb.group({
      id: [''],
      name: ['', Validators.required],
      type: ['', Validators.required],
      ssh: this.sshForms,
      winrm: this.winrmForms,
      sudo: this.sudoForms
    });

    this.store.select(routeParams).pipe(
      pluck('id'),
      filter(identity),
      takeUntil(this.isDestroyed))
      .subscribe((id: string) => {
        this.id = id;
        this.store.dispatch(new GetNodeCredential({
          id: id
        }));
      });

    combineLatest([
      this.store.select(getStatus),
      this.store.select(credentialFromRoute)
    ]).pipe(
        filter(([status, nodeCredential]) =>
          status === EntityStatus.loadingSuccess && !isNil(nodeCredential)),
        takeUntil(this.isDestroyed))
      .subscribe(([status, nodeCredential]) => {
        this.isLoading =
          !allLoaded([status]);
        this.nodeCredential = nodeCredential;
        const data = this.nodeCredential.data;

        setTimeout(() => (
          this.updateForm.controls.name.setValue(this.nodeCredential.name),
          this.updateForm.controls.type.setValue(this.nodeCredential.type),
          this.updateForm.controls.id.setValue(this.nodeCredential.id),

          this.resetForm.controls.name.setValue(this.nodeCredential.name),
          this.resetForm.controls.type.setValue(this.nodeCredential.type),
          this.resetForm.controls.id.setValue(this.nodeCredential.id),
          this.nodeDetailsLoading = false
        ),
          200);
          if (this.nodeCredential.type === 'sudo') {
            data.filter(datum => datum.key === 'options')
              .forEach(datum => this.sudoForms.controls.options.setValue(datum.value));
          }

      });

      this.store.pipe(select(updateStatus),
      takeUntil(this.isDestroyed),
      filter(state => this.saveInProgress || this.resetInProgress && !pending(state)))
      .subscribe((state) => {
        if (this.tabValue === 'details') {
          this.saveInProgress = false;
          this.saveSuccessful = (state === EntityStatus.loadingSuccess);
          this.updateForm.markAsPristine();
        } else {
          this.resetInProgress = false;
          this.resetSuccessful = (state === EntityStatus.loadingSuccess);
          this.resetForm.markAsPristine();
        }
      });
  }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Settings);
    this.store.select(routeURL).pipe(takeUntil(this.isDestroyed))
      .subscribe((url: string) => {
        this.url = url;
        const [, fragment] = url.split('#');
        this.tabValue = (fragment === 'reset') ? 'reset' : 'details';
      });
  }


  onSelectedTab(event: { target: { value: NodeCredentialTabName } }) {
    this.tabValue = event.target.value;
    this.router.navigate([this.url.split('#')[0]], { fragment: event.target.value });
  }

  selectChangeHandlers(id: string): void {
    this.passwordSelected = id;
  }

  saveNodeCredential(data: NodeObject): void {
    if (this.tabValue === 'details') {
      this.saveSuccessful = false;
      this.saveInProgress = true;
      this.nodeCredential.name = data.name;
    } else {
      this.resetSuccessful = false;
      this.resetInProgress = true;
      this.nodeCredential = this.saveCred.getNodeCredentialCreate(data);
    }
    if (!this.checkError(
        'password',
        data.type === 'ssh' ? this.sshForms
        : data.type === 'winrm' ? this.winrmForms
        : this.sudoForms
      ) || (!this.checkError(
        'username',
        data.type === 'ssh' ? this.sshForms
        : data.type === 'winrm' ? this.winrmForms
        : ''
    ))) {
      this.store.dispatch(new UpdateNodeCredential(this.nodeCredential));
      if (data.type !== 'sudo') {
        this.resetForm.controls[data.type]['controls']['username'].setValue('');
      }
      this.resetForm.controls[data.type]['controls']['password'].setValue('');
    }
}

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  checkError(field: string , type: any) {
    return (((type.get(field).hasError('required') || type.get(field).hasError('pattern'))
    && type.get(field).dirty) && type.get(field).value !== '');
  }
}
