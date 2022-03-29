import { Component, EventEmitter, Input, OnDestroy, OnInit } from '@angular/core';
import { Store } from '@ngrx/store';
import { takeUntil } from 'rxjs/operators';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { combineLatest, Subject } from 'rxjs';
import { ResetUserKey } from 'app/entities/org-users/org-users.action';
import { Utilities } from 'app/helpers/utilities/utilities';
import { saveAs } from 'file-saver';
import { ResetKey } from 'app/entities/clients/client.model';
import { isNil } from 'lodash';
import { EntityStatus } from 'app/entities/entities';
import { resetError, resetStatus, resetUserKey } from 'app/entities/org-users/org-users.selectors';

@Component({
  selector: 'app-reset-user-key',
  templateUrl: './reset-user-key.component.html',
  styleUrls: ['./reset-user-key.component.scss']
})

export class ResetUserKeyComponent implements OnInit, OnDestroy {
  @Input() openEvent: EventEmitter<boolean>;
  @Input() serverId: string;
  @Input() orgId: string;
  @Input() name: string;

  public isReset = false;
  public resetting = false;
  public error: string;
  public publicKey: string;
  public privateKey: string;
  public org: string;
  public server: string;
  public visible = false;
  public close = new EventEmitter();
  private isDestroyed = new Subject<boolean>();

  constructor(
    private store: Store<NgrxStateAtom>
  ) { }

  ngOnInit(): void {
    this.openEvent.pipe(takeUntil(this.isDestroyed))
      .subscribe(() => {
        this.visible = true;
        this.resetting = false;
        this.isReset = false;
        this.server = this.serverId;
        this.org = this.orgId;
        this.name = this.name;
        this.error = '';
        this.privateKey = '';
      });

      combineLatest([
        this.store.select(resetStatus),
        this.store.select(resetUserKey),
        this.store.select(resetError)
      ]).pipe(
        takeUntil(this.isDestroyed))
        .subscribe(([resetStatusSt, resetKeyState, errorSt]) => {
          if (resetStatusSt === EntityStatus.loadingSuccess &&
          !isNil(resetKeyState)) {
              this.resetting = false;
              this.isReset = true;
              this.privateKey = resetKeyState?.user_key;
          } else if (resetStatusSt === EntityStatus.loadingFailure) {
              this.error = errorSt;
              this.closeCreateModal();
          }
        });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  handleInput(event: KeyboardEvent): void {
    if (Utilities.isNavigationKey(event)) {
      return;
    }
  }

  closeCreateModal(): void {
    this.resetCreateModal();
    this.visible = false;
  }

  private resetCreateModal(): void {
    this.resetting = false;
    this.isReset = false;
    this.error = '';
    this.privateKey = '';
  }

  resetKeyUser(): void {
    this.resetting = true;
    const payload: ResetKey = {
      'org_id': this.orgId,
      'server_id': this.serverId,
      'name': this.name
    };
    this.store.dispatch(new ResetUserKey(payload));
  }

  downloadKey(): void {
    const template = `
    Private RSA Key

    ${this.privateKey}
    `;

    const blob = new Blob([template], { type: 'text/plain;charset=utf-8' });
    saveAs(blob, this.name + '.pem');
  }
}
