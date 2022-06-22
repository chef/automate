import { Component, EventEmitter, Input, OnDestroy, OnInit } from '@angular/core';
import { Store } from '@ngrx/store';
import { takeUntil } from 'rxjs/operators';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { combineLatest, Subject } from 'rxjs';
import { ResetKeyClient } from 'app/entities/clients/client.action';
import { getStatus,
  resetKeyClient,
  saveError } from 'app/entities/clients/client-details.selectors';
import { EntityStatus } from 'app/entities/entities';
import { Utilities } from 'app/helpers/utilities/utilities';
import { isNil } from 'lodash/fp';
import { saveAs } from 'file-saver';
import { ResetKey } from 'app/entities/clients/client.model';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

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
    private store: Store<NgrxStateAtom>,
    private telemetryService: TelemetryService
  ) { }

  ngOnInit() {
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
      this.store.select(getStatus),
      this.store.select(resetKeyClient),
      this.store.select(saveError)
    ]).pipe(
      takeUntil(this.isDestroyed))
      .subscribe(([getStatusSt, resetKeyState, errorSt]) => {
        if (getStatusSt === EntityStatus.loadingSuccess &&
        !isNil(resetKeyState)) {
            this.resetting = false;
            this.isReset = true;
            this.privateKey = resetKeyState?.client_key.private_key;
        } else if (getStatusSt === EntityStatus.loadingFailure) {
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

  resetKeyClient(): void {
    this.resetting = true;
    const payload: ResetKey = {
      'org_id': this.orgId,
      'server_id': this.serverId,
      'name': this.name
    };
    this.store.dispatch(new ResetKeyClient(payload));
    this.telemetryService.track('InfraServer_Clients_ResetKey');
  }

  downloadKey(): void {
    const template = `
    Private RSA Key

    ${this.privateKey}
    `;

    const blob = new Blob([template], { type: 'text/plain;charset=utf-8' });
    saveAs(blob, this.name + '.pem');
    this.telemetryService.track('InfraServer_Clients_Download_ResetKey');
  }

}
