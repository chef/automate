import { Component, OnInit, OnDestroy } from '@angular/core';
import { FormBuilder, FormGroup, Validators, FormControl } from '@angular/forms';
import { Store, select } from '@ngrx/store';
import { identity } from 'lodash/fp';
import { Subject } from 'rxjs';
import { filter, pluck, takeUntil } from 'rxjs/operators';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { routeParams } from 'app/route.selectors';
import { Regex } from 'app/helpers/auth/regex';
import { loading } from 'app/entities/entities';
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
  public firstLoad = true;

  constructor(
    private store: Store<NgrxStateAtom>,
    fb: FormBuilder
  ) {
    const initialStatus: TokenStatus = 'active';
    this.updateForm = fb.group({
      // Must stay in sync with error checks in api-token-details.component.html
      name: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      status: [initialStatus, Validators.required]
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

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public handleChange(): void {
    if (this.nameCtrl.value === this.token.name) {
      this.nameCtrl.markAsPristine();
    }
  }

  public saveChange(): void {
    this.firstLoad = false;
    this.saveInProgress = true;
    const name: string = this.updateForm.controls.name.value.trim();
    const active: boolean = <TokenStatus>this.updateForm.controls.status.value === 'active';
    const status: TokenStatus = active ? 'active' : 'inactive';
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
          this.updateForm.reset({ name, status });
        }
      });
  }

  get nameCtrl(): FormControl {
    return <FormControl>this.updateForm.controls.name;
  }
}
