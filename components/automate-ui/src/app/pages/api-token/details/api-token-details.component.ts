import { Component, OnInit, OnDestroy } from '@angular/core';
import { FormBuilder, FormGroup, Validators, FormControl } from '@angular/forms';
import { Store, select } from '@ngrx/store';
import { identity } from 'lodash/fp';
import { Subject } from 'rxjs';
import { filter, pluck, takeUntil } from 'rxjs/operators';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { routeParams } from 'app/route.selectors';
import { Regex } from 'app/helpers/auth/regex';
import { loading, EntityStatus } from 'app/entities/entities';
import { GetToken, UpdateToken } from 'app/entities/api-tokens/api-token.actions';
import { apiTokenFromRoute, updateStatus } from 'app/entities/api-tokens/api-token.selectors';
import { ApiToken } from 'app/entities/api-tokens/api-token.model';

type TokenStatus = 'active' | 'inactive';
type TokenTabName = 'details';

@Component({
  selector: 'app-api-token-details',
  templateUrl: './api-token-details.component.html',
  styleUrls: ['./api-token-details.component.scss']
})
export class ApiTokenDetailsComponent implements OnInit, OnDestroy {
  public tabValue: TokenTabName = 'details';
  public token: ApiToken;
  public status: TokenStatus;
  private isDestroyed: Subject<boolean> = new Subject<boolean>();
  public updateForm: FormGroup;
  public saveInProgress = false;
  public saveSuccessful = false;

  constructor(
    private store: Store<NgrxStateAtom>,
    fb: FormBuilder
  ) {
    const initialStatus: TokenStatus = 'active';
    this.updateForm = fb.group({
      // Must stay in sync with error checks in api-token-details.component.html
      name: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      status: [initialStatus]
    });
  }

  ngOnInit(): void {
    this.store.pipe(
      select(apiTokenFromRoute),
      filter(identity),
      takeUntil(this.isDestroyed))
      .subscribe((state) => {
        this.token = { ...state };
        this.updateForm.controls.name.setValue(this.token.name);
        this.status = this.token.active ? 'active' : 'inactive';
        this.updateForm.controls.status.setValue(this.status);
      });

    this.store.pipe(
      select(routeParams),
      pluck('id'),
      filter(identity),
      takeUntil(this.isDestroyed))
      .subscribe((id: string) => {
        this.store.dispatch(new GetToken({ id }));
      });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public saveToken(): void {
    this.saveSuccessful = false;
    this.saveInProgress = true;
    const name: string = this.updateForm.controls.name.value.trim();
    const active = <TokenStatus>this.updateForm.controls.status.value === 'active';
    const token: ApiToken = { ...this.token, name, active };
    this.store.dispatch(new UpdateToken({ token }));

    const pendingSave = new Subject<boolean>();
    this.store.pipe(
      select(updateStatus),
      filter(identity),
      takeUntil(pendingSave))
      .subscribe((state) => {
        if (!loading(state)) {
          pendingSave.next(true);
          pendingSave.complete();
          this.saveInProgress = false;
          this.saveSuccessful = (state === EntityStatus.loadingSuccess);
          if (this.saveSuccessful) {
            this.updateForm.markAsPristine();
          }
        }
      });
  }

  public get nameCtrl(): FormControl {
    return <FormControl>this.updateForm.controls.name;
  }
}
