import { Component, OnInit, OnDestroy } from '@angular/core';
import { Store } from '@ngrx/store';
import { HttpClient } from '@angular/common/http';
import { environment as env } from 'environments/environment';
import { Observable, Subject, combineLatest } from 'rxjs';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { routeURL, routeParams } from 'app/route.selectors';
import { filter, takeUntil, pluck } from 'rxjs/operators';
import { identity } from 'lodash/fp';
import { CookbookVersions } from 'app/entities/cookbooks/cookbookversions.model';
import { allCookbookVersions } from 'app/entities/cookbooks/cookbookversions.selectors';
import { GetCookbookVersions } from 'app/entities/cookbooks/cookbookversions.actions';
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
export class CookbookDetailsComponent implements OnInit, OnDestroy {
  public loading$: Observable<boolean>;
  private isDestroyed = new Subject<boolean>();
  public cookbook: CookbookVersions;
  public saveSuccessful = false;
  public saving = false;
  public isLoading = true;
  public url: string;
  public serverId: string;
  public orgId: string;
  public cookbookName: string;
  public cookbookDetails: CookbookDetails;
  public tabValue: CookbookDetailsTab = 'details';
  public readFile: RootFiles;
  public readFileUrl: string;
  public readFileContent;
  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService,
    private http: HttpClient
  ) {
      this.store.select(allCookbookVersions)
      .subscribe((cookbookWithVersions) => {
        this.cookbook = cookbookWithVersions[0];
        if ( this.cookbook && this.serverId && this.orgId) {
          this.store.dispatch(new GetCookbookDetailsForVersion({
            server_id: this.serverId,
            org_id: this.orgId,
            cookbook_name: this.cookbook.name,
            cookbook_version: this.cookbook.versions[0]
          }));
          this.onCookbookVersionChange(
            this.serverId, this.orgId,
            this.cookbook.name,
            this.cookbook.versions[0]
          );
        }
      });
  }

  ngOnInit() {

    this.layoutFacade.showSidebar(Sidebar.Infrastructure);
    this.store.select(routeURL).pipe()
    .subscribe((url: string) => {
      this.url = url;
      const [, fragment] = url.split('#');
      this.tabValue = (fragment === 'details') ? 'details' : 'content';
    });

    combineLatest([
      this.store.select(routeParams).pipe(pluck('id'), filter(identity)),
      this.store.select(routeParams).pipe(pluck('orgid'), filter(identity)),
      this.store.select(routeParams).pipe(pluck('name'), filter(identity))
    ]).pipe(
        takeUntil(this.isDestroyed)
        ).subscribe(([ server_id, org_id, cookbook_name ]) => {
          this.serverId = server_id;
          this.orgId = org_id;
          this.cookbookName = cookbook_name;
          this.store.dispatch(new GetCookbookVersions({
            server_id: server_id,
            org_id: org_id,
            cookbook_name: cookbook_name
          }));
      });
  }

  public onCookbookVersionChange(
    server_id: string,
    org_id: string,
    cookbook_name: string,
    cookbook_version: string
    ): void {
    combineLatest([
      this.store.select(getAllCookbooksDetailsForVersionStatus),
      this.store.select(allCookbookDetails)
    ]).pipe().subscribe(([_getCookbooksSt, allCookbooksState]) => {
        this.cookbookDetails = allCookbooksState[0];
        this.readFile = allCookbooksState[0]?.root_files.find(data => data.name === 'README.md');
        if ( this.readFile ) {
          this.readFileUrl = encodeURIComponent(this.readFile?.url);
          this.http.get(
            `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/cookbooks/${cookbook_name}/${cookbook_version}/file-content?url=${this.readFileUrl}`).subscribe
            (fileContent => {
              this.readFileContent = fileContent;
            });
        }
      });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }
}
