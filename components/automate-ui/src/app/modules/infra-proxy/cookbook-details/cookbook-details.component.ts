import { Component, OnInit, OnDestroy } from '@angular/core';
import { Store } from '@ngrx/store';
import { HttpClient } from '@angular/common/http';
import { environment as env } from 'environments/environment';
import { Subject, combineLatest } from 'rxjs';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { EntityStatus } from 'app/entities/entities';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { routeURL, routeParams } from 'app/route.selectors';
import { filter, takeUntil, pluck } from 'rxjs/operators';
import { identity, isNil } from 'lodash/fp';
import { CookbookVersions } from 'app/entities/cookbooks/cookbook-versions.model';
import { allCookbookVersions, getStatus } from 'app/entities/cookbooks/cookbook-versions.selectors';
import { GetCookbookVersions } from 'app/entities/cookbooks/cookbook-versions.actions';
import { CookbookDetails, RootFiles } from 'app/entities/cookbooks/cookbook-details.model';
import {
  allCookbookDetails,
  getStatus as getAllCookbooksDetailsForVersionStatus
} from 'app/entities/cookbooks/cookbook-details.selectors';
import { GetCookbookDetails } from 'app/entities/cookbooks/cookbook-details.actions';
export type CookbookDetailsTab = 'details' | 'content';

@Component({
  selector: 'app-cookbook-details',
  templateUrl: './cookbook-details.component.html',
  styleUrls: ['./cookbook-details.component.scss']
})
export class CookbookDetailsComponent implements OnInit, OnDestroy {
  private isDestroyed = new Subject<boolean>();
  public cookbook: CookbookVersions;
  public url: string;
  public serverId: string;
  public orgId: string;
  public cookbookName: string;
  public cookbookDetails: CookbookDetails;
  public tabValue: CookbookDetailsTab = 'details';
  public readFile: RootFiles;
  public readFileUrl: string;
  public readFileContent;
  public cookbookDetailsLoading = true;
  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService,
    private http: HttpClient
  ) { }

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
    ).subscribe(([server_id, org_id, cookbook_name]) => {
      this.serverId = server_id;
      this.orgId = org_id;
      this.cookbookName = cookbook_name;
      this.store.dispatch(new GetCookbookVersions({
        server_id: server_id,
        org_id: org_id,
        cookbook_name: cookbook_name
      }));
    });

    combineLatest([
      this.store.select(getStatus),
      this.store.select(allCookbookVersions)
    ]).pipe(
      filter(([getCookbookVersionSt, _cookbookVersionState]) =>
      getCookbookVersionSt === EntityStatus.loadingSuccess),
      filter(([_getCookbookVersionSt, cookbookVersionState]) =>
        !isNil(cookbookVersionState[0])),
      takeUntil(this.isDestroyed))
      .subscribe(([_getCookbookVersionSt, cookbookVersionState]) => {
        this.cookbook = cookbookVersionState[0];
        this.store.dispatch(new GetCookbookDetails({
          server_id: this.serverId,
          org_id: this.orgId,
          cookbook_name: this.cookbook.name,
          cookbook_version: this.cookbook.versions[0]
        }));
        this.onCookbookVersionChange(
          this.serverId,
          this.orgId,
          this.cookbook.name,
          this.cookbook.versions[0]
        );
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
    ]).pipe(
      filter(([getCookbooksSt, _allCookbooksState]) =>
      getCookbooksSt === EntityStatus.loadingSuccess),
      filter(([_getCookbooksSt, allCookbooksState]) =>
        !isNil(allCookbooksState[0])),
      takeUntil(this.isDestroyed))
      .subscribe(([_getCookbooksSt, allCookbooksState]) => {
      this.cookbookDetails = allCookbooksState[0];
      this.readFile = allCookbooksState[0]?.root_files.find(data => data.name === 'README.md');
      if (this.readFile) {
        this.readFileUrl = encodeURIComponent(this.readFile?.url);
        this.http.get(
          `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/cookbooks/${cookbook_name}/${cookbook_version}/file-content?url=${this.readFileUrl}`).subscribe
          (fileContent => {
            this.readFileContent = fileContent;
            this.cookbookDetailsLoading = false;
          });
      }
    });
  }

  public handleCookbookVersionChange(
    server_id: string,
    org_id: string,
    cookbook_name: string,
    event
  ): void {
    this.readFileContent = '';
    this.cookbookDetailsLoading = true;
    this.onCookbookVersionChange(server_id, org_id, cookbook_name, event.target.value);
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }
}
