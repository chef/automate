import { Component, OnInit, OnDestroy } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { Store } from '@ngrx/store';
import { Subject } from 'rxjs';
import { takeUntil } from 'rxjs/operators';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { routeURL } from 'app/route.selectors';
import { Regex } from 'app/helpers/auth/regex';
import { LayoutFacadeService } from 'app/entities/layout/layout.facade';
export type ChefServerTabName = 'orgs' | 'details';

@Component({
  selector: 'app-chef-servers-details',
  templateUrl: './chef-servers-details.component.html',
  styleUrls: ['./chef-servers-details.component.scss']
})
export class ChefServersDetailsComponent implements OnInit, OnDestroy {
  public tabValue: ChefServerTabName = 'orgs';
  public url: string;
  public chefServerForm: FormGroup;
  public createModalVisible = false;
  public modalType: string;

  // isLoading represents the initial load as well as subsequent updates in progress.
  public isLoading = true;
  public saving = false;
  private isDestroyed = new Subject<boolean>();

  constructor(
    private fb: FormBuilder,
    private store: Store<NgrxStateAtom>,
    private router: Router,
    private layoutFacade: LayoutFacadeService

  ) { }

  ngOnInit() {
    this.layoutFacade.showInfastructureSidebar();
    // Populate our tabValue from the fragment.
    this.store.select(routeURL).pipe(takeUntil(this.isDestroyed))
    .subscribe((url: string) => {
      this.url = url;
      const [, fragment] = url.split('#');
      this.tabValue = (fragment === 'details') ? 'details' : 'orgs';
    });

    this.chefServerForm = this.fb.group({
      name: ['chef-server', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      fqdn: ['chef.io', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      ip_address: ['198.162.0', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]]
    });

  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  onSelectedTab(event: { target: { value: ChefServerTabName } }) {
    this.tabValue = event.target.value;
    this.router.navigate([this.url.split('#')[0]], { fragment: event.target.value });
  }

  public openCreateModal(type: string): void {
    this.createModalVisible = true;
    this.modalType = type;
  }

  public closeCreateModal(): void {
    this.createModalVisible = false;
  }

}
