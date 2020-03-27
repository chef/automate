import { Component, OnInit } from '@angular/core';
import { Store } from '@ngrx/store';
// import { Router } from '@angular/router';
import { HttpClient } from '@angular/common/http';
import { environment as env } from 'environments/environment';
import { Observable, Subject, combineLatest } from 'rxjs';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { routeURL, routeParams } from 'app/route.selectors';
import { filter, takeUntil, pluck } from 'rxjs/operators';
import { identity, isNil } from 'lodash/fp';
// import { EntityStatus } from 'app/entities/entities';
import { Cookbook } from 'app/entities/cookbooks/cookbook.model';
import {
  cookbookFromRoute
} from 'app/entities/cookbooks/cookbook.selectors';
import { CookbookDetails, RootFiles } from 'app/entities/cookbooks/cookbookdetails.model';
import {
  allCookbookDetails,
  getStatus as getAllCookbooksDetailsForVersionStatus
} from 'app/entities/cookbooks/cookbookdetails.selectors';
import { GetCookbookDetailsForVersion } from 'app/entities/cookbooks/cookbookdetails.actions';
export type CookbookDetailsTab = 'details' | 'content';

@Component({
  selector: 'app-cookbook-details',
  templateUrl: './cookbook-details.component.html',
  styleUrls: ['./cookbook-details.component.scss']
})
export class CookbookDetailsComponent implements OnInit {
  public loading$: Observable<boolean>;
  private isDestroyed = new Subject<boolean>();
  public cookbooks: Cookbook;
  public saveSuccessful = false;
  public saving = false;
  public isLoading = true;
  public url: string;
  public serverId: string;
  public orgId: string;
  public cookbookDetails: CookbookDetails;
  public tabValue: CookbookDetailsTab = 'details';
  public readFile: RootFiles;
  public readFileUrl: string;
  public readFileContent;
  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService,
    private http: HttpClient
  ) {}

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);
    this.store.select(routeURL).pipe()
    .subscribe((url: string) => {
      this.url = url;
      const [, fragment] = url.split('#');
      this.tabValue = (fragment === 'details') ? 'details' : 'content';
    });

    // this.store.select(allCookbooks).pipe(
    //   filter(identity),
    //   takeUntil(this.isDestroyed))
    //   .subscribe((state) => {
    //     this.cookbookDetails = { ...state };
    //     console.log(this.cookbookDetails);
    //   });

    // this.store.select(routeParams).pipe(
    //   takeUntil(this.isDestroyed),
    //   pluck('id'),
    //   filter(identity),
    //   distinctUntilChanged()
    // ).subscribe((id: string) => this.store.dispatch(new GetPolicy({ id })));

    // combineLatest([
    //   this.store.select(getAllCookbooksForOrgStatus)
    // ]).pipe(
    //   takeUntil(this.isDestroyed)
    // ).subscribe(([getCookbooksSt]) => {
    //     this.isLoading = !allLoaded([getCookbooksSt]);
    //   });

    combineLatest([
      this.store.select(cookbookFromRoute),
      this.store.select(routeParams).pipe(pluck('id'), filter(identity)),
      this.store.select(routeParams).pipe(pluck('orgid'), filter(identity))
    ]).pipe(
        filter(([allCookbooksState]) =>
          !isNil(allCookbooksState)),
        takeUntil(this.isDestroyed)
        ).subscribe(([allCookbooksState, server_id, org_id ]) => {
          this.serverId = server_id;
          this.orgId = org_id;
          this.cookbooks = allCookbooksState;
          this.store.dispatch(new GetCookbookDetailsForVersion({
            server_id: server_id,
            org_id: org_id,
            cookbook_name: this.cookbooks.name,
            cookbook_version: this.cookbooks.current_version
          }));
          this.onCookbookVersionChange(this.serverId, this.orgId, this.cookbooks.name, this.cookbooks.current_version);
      });
  }

  public onCookbookVersionChange(server_id:string, org_id:string, cookbook_name:string, cookbook_version:string): void {
    combineLatest([
      this.store.select(getAllCookbooksDetailsForVersionStatus),
      this.store.select(allCookbookDetails)
    ]).pipe().subscribe(([_getCookbooksSt, allCookbooksState]) => {
        this.cookbookDetails = allCookbooksState[0];
        this.readFile = allCookbooksState[0]?.root_files.find(data => data.name === 'README.md');
        if(this.readFile) {
          this.readFileUrl = encodeURIComponent(this.readFile?.url);
          this.http.get(
            `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/cookbooks/${cookbook_name}/${cookbook_version}/file-content?url=${this.readFileUrl}`).subscribe
            (fileContent => {
              this.readFileContent = fileContent;
            });
        }        
      });
  }

  // onSelectedTab(event: { target: { value: CookbookDetailsTab } }) {
  //   this.tabValue = event.target.value;
  //   this.router.navigate([this.url.split('#')[0]], { fragment: event.target.value });
  // }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }
}
