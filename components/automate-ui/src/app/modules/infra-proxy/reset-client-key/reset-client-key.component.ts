import { Component, EventEmitter, Input, OnDestroy, OnInit } from '@angular/core';
import { Store } from '@ngrx/store';
import { takeUntil } from 'rxjs/operators';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { combineLatest, Subject } from 'rxjs';
import { ResetKeyClient } from 'app/entities/clients/client.action';
import { getStatus, resetKeyClient, saveError } from 'app/entities/clients/client-details.selectors';
import { EntityStatus } from 'app/entities/entities';
import { isNil } from 'lodash/fp';
import { saveAs } from 'file-saver';

@Component({
  selector: 'app-reset-client-key',
  templateUrl: './reset-client-key.component.html',
  styleUrls: ['./reset-client-key.component.scss']
})

export class ResetClientKeyComponent implements OnInit, OnDestroy {
  @Input() openEvent: EventEmitter<boolean>;
  @Input() serverId: string;
  @Input() orgId: string;
  @Input() name: string;

  public authFailure = false;
  public isReset = false;
  public reseting = false;
  public conflictError = false;
  public error: string;
  public publicKey: string;
  public privateKey: string;
  public org: string;
  public server: string;
  public visible = false;
  public conflictErrorEvent = new EventEmitter<boolean>();
  public close = new EventEmitter();
  private isDestroyed = new Subject<boolean>();

  constructor(
    private store: Store<NgrxStateAtom>
  ) { }

  ngOnInit() {
    this.openEvent.pipe(takeUntil(this.isDestroyed))
      .subscribe(() => {
        this.conflictError = false;
        this.visible = true;
        this.server = this.serverId;
        this.org = this.orgId;
        this.name = this.name;
        this.error = '';
      });

    combineLatest([
      this.store.select(getStatus),
      this.store.select(resetKeyClient),
      this.store.select(saveError)
    ]).pipe(
      takeUntil(this.isDestroyed))
      .subscribe(([getStatusSt, resetKeyState, errorSt]) => {
      if (getStatusSt === EntityStatus.loadingSuccess && 
        !isNil(resetKeyState)) {        
            this.reseting = false;
            this.isReset = true;
            this.conflictError = false;
            this.privateKey = resetKeyState?.private_key;
          } else if (getStatusSt === EntityStatus.loadingFailure) {            
            this.error = errorSt;
            if(this.error ==='missing update permission') {
              this.isReset = false;
              this.reseting = false;
              this.conflictError = true;
            } else {
              this.closeCreateModal();
            }
            this.authFailure = true;
          }
        });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public handleInput(event: KeyboardEvent): void {
    if (this.isNavigationKey(event)) {
      return;
    }
    this.conflictError = false;
  }

  closeCreateModal(): void {
    this.resetCreateModal();
    this.visible = false;
  }

  private resetCreateModal(): void {
    this.reseting = false;
    this.isReset = false;
    this.conflictError = false;
    this.error = '';
    this.privateKey = '';
    this.conflictErrorEvent.emit(false);
  }

  resetKeyClient(): void {
    this.reseting = true;
    const payload = {
      'org_id': this.orgId,
      'server_id': this.serverId,
      'name': this.name
    };
    this.store.dispatch(new ResetKeyClient(payload));
  }

  downloadKey() {
    const template = `
    Private RSA Key
    ${this.publicKey}
    `;

    const blob = new Blob([template], { type: 'text/plain;charset=utf-8' });
    saveAs(blob, this.name + '.pem');
  }

  private isNavigationKey(event: KeyboardEvent): boolean {
    return event.key === 'Shift' || event.key === 'Tab';
  }
}
