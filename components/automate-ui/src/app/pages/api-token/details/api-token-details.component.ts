import { Component, OnInit, OnDestroy } from '@angular/core';
import { Store, select } from '@ngrx/store';
import { identity } from 'lodash/fp';
import { Subject } from 'rxjs';
import { filter, pluck, takeUntil } from 'rxjs/operators';
import {
  FormBuilder,
  FormGroup,
  Validators
} from '@angular/forms';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { routeParams } from 'app/route.selectors';
import { loading } from 'app/entities/entities';
import { GetToken, UpdateToken } from 'app/entities/api-tokens/api-token.actions';
import { apiTokenFromRoute, updateStatus } from 'app/entities/api-tokens/api-token.selectors';
import { ApiToken } from 'app/entities/api-tokens/api-token.model';

@Component({
  selector: 'app-api-token-details',
  templateUrl: './api-token-details.component.html',
  styleUrls: ['./api-token-details.component.scss']
})
export class ApiTokenDetailsComponent implements OnInit, OnDestroy {
  public tabValue = 'details';
  public token: ApiToken;
  public status: string;
  private isDestroyed: Subject<boolean> = new Subject<boolean>();
  public updateNameForm: FormGroup;
  public disableSave = true;
  public saveInProgress = false;

  constructor(private store: Store<NgrxStateAtom>,
              fb: FormBuilder
              ) {
      this.updateNameForm = fb.group({
        // Must stay in sync with error checks in api-token-details.component.html
        name: ['', Validators.required]
      });
    }

    ngOnInit(): void {
      this.store.pipe(
        select(apiTokenFromRoute),
        filter(identity),
        takeUntil(this.isDestroyed))
        .subscribe((state) => {
          this.token = { ...state };
          this.status = this.token.active ? 'Active' : 'Inactive';
          this.updateNameForm.controls['name'].setValue(this.token.name);
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

    public handleNameChange(): void {
      this.disableSave = this.updateNameForm.controls['name'].value === this.token.name;
    }

    public saveNameChange(): void {
      this.saveInProgress = true;
      const name = this.updateNameForm.controls['name'].value.trim();
      const token: ApiToken = { ...this.token, name  };
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
            this.disableSave = true;
          }
        });
    }

}
