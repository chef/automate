import { Component, OnInit, OnDestroy } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Store } from '@ngrx/store';
import { Subject, combineLatest } from 'rxjs';
import { first, filter, takeUntil, pluck } from 'rxjs/operators';
import { identity, isNil } from 'lodash/fp';

import { environment as env } from 'environments/environment';
import { CollapsibleListMapper } from 'app/helpers/infra-proxy/collapsible-list-mapper';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { EntityStatus } from 'app/entities/entities';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { routeURL, routeParams } from 'app/route.selectors';
import { CookbookVersions } from 'app/entities/cookbooks/cookbook-versions.model';
import { cookbookVersionsFromRoute, getStatus } from 'app/entities/cookbooks/cookbook-versions.selectors';
import { GetCookbookVersions } from 'app/entities/cookbooks/cookbook-versions.actions';
import {
  CookbookDetails,
  RootFiles,
  Menu,
  SubMenu
} from 'app/entities/cookbooks/cookbook-details.model';
import {
  cookbookDetailsFromRoute,
  getStatus as getAllCookbooksDetailsStatus
} from 'app/entities/cookbooks/cookbook-details.selectors';
import { GetCookbookDetails } from 'app/entities/cookbooks/cookbook-details.actions';
export type CookbookDetailsTab = 'content' | 'details';

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
  public currentVersion: string;
  public cookbookName: string;
  public cookbookDetails: CookbookDetails;
  public tabValue: CookbookDetailsTab = 'content';
  public readFile: RootFiles;
  public readFileUrl: string;
  public readFileContent;
  public cookbookDetailsLoading = false;
  public cookbookVersionsLoading = true;
  public contentTabLoading = false;
  public contentLoading = false;
  public status = false;
  public menuList: Menu[] = [];
  public defaultContent: SubMenu;
  public contentUrl: string;
  public urlContent;
  public contentData: string;
  public activeContentName: string;
  public menuIndex: number;
  public listItem = [
    'attributes',
    'definitions',
    'files',
    'libraries',
    'providers',
    'recipes',
    'resources',
    'root_files',
    'templates'
  ];

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
      this.store.select(routeParams).pipe(pluck('org-id'), filter(identity)),
      this.store.select(routeParams).pipe(pluck('cookbook-name'), filter(identity))
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
      this.store.select(cookbookVersionsFromRoute)
    ]).pipe(
      filter(([getCookbookVersionSt, _cookbookVersionState]) =>
        getCookbookVersionSt === EntityStatus.loadingSuccess),
      filter(([_getCookbookVersionSt, cookbookVersionState]) =>
        !isNil(cookbookVersionState)),
      takeUntil(this.isDestroyed))
      .subscribe(([_getCookbookVersionSt, cookbookVersionState]) => {
        this.cookbook = cookbookVersionState;
        this.cookbookVersionsLoading = false;
        this.cookbookDetailsLoading = true;
        this.contentTabLoading = true;
        this.currentVersion = this.cookbook.versions[0];
        this.store.dispatch(new GetCookbookDetails({
          server_id: this.serverId,
          org_id: this.orgId,
          cookbook_name: this.cookbook.name,
          cookbook_version: this.currentVersion
        }));
      });

    combineLatest([
      this.store.select(getAllCookbooksDetailsStatus),
      this.store.select(cookbookDetailsFromRoute)
    ]).pipe(
      filter(([getCookbooksSt, _cookbookDetailsState]) =>
        getCookbooksSt === EntityStatus.loadingSuccess),
      filter(([_getCookbooksSt, cookbookDetailsState]) =>
        !isNil(cookbookDetailsState)),
      takeUntil(this.isDestroyed))
      .subscribe(([_getCookbooksSt, cookbookDetailsState]) => {
        this.cookbookDetails = cookbookDetailsState;
        this.menuList = CollapsibleListMapper.transform(cookbookDetailsState, this.listItem);
        if (this.menuList.length) {
          // check submenu exists and show default item details
          const menus = this.menuList.filter(menu => menu.subMenu.length);
          if ( menus.length ) {
            this.defaultContent = menus[0].subMenu[0];
            this.getContent(this.defaultContent);
            this.menuIndex = this.menuList.indexOf(menus[0]);
          }

          if ( this.menuList[0].subMenu.length ) {
            this.defaultContent = this.menuList[0].subMenu[0];
            this.getContent(this.defaultContent);
          }
        }
        this.readFile = cookbookDetailsState?.root_files.find(data => data.name === 'README.md');
        if (this.readFile) {
          this.readFileUrl = encodeURIComponent(this.readFile?.url);
          this.http.get(
            `${env.infra_proxy_url}/servers/${this.serverId}/orgs/${this.orgId}/cookbooks/${this.cookbookName}/${this.currentVersion}/file-content?url=${this.readFileUrl}`)
            .pipe(first())
            .subscribe
            (fileContent => {
              this.readFileContent = fileContent;
              this.cookbookDetailsLoading = false;
            });
        }
      });

  }

  public handleCookbookVersionChange(event): void {
    this.readFileContent = '';
    this.cookbookDetailsLoading = true;
    this.contentTabLoading = true;
    this.store.dispatch(new GetCookbookDetails({
      server_id: this.serverId,
      org_id: this.orgId,
      cookbook_name: this.cookbook.name,
      cookbook_version: event.target.value
    }));
  }

  listClickEvent(event) {
    if (event.target.classList.contains('extend-list')) {
      event.target.classList.remove('extend-list');
      event.target.parentNode.querySelector('ul').classList.remove('show');
    } else {
      event.target.classList.add('extend-list');
      event.target.parentNode.querySelector('ul').classList.add('show');
    }
  }

  subListClickEvent(event, currentContent: SubMenu) {
    this.contentLoading = true;
    this.getContent(currentContent);
    document.querySelector('li.active-sub-list').classList.remove('active-sub-list');
    event.target.classList.add('active-sub-list');
  }

  getContent(activeContent: SubMenu) {
    this.activeContentName = activeContent.name;
    this.contentUrl = encodeURIComponent(activeContent.url);
    this.http.get(
      `${env.infra_proxy_url}/servers/${this.serverId}/orgs/${this.orgId}/cookbooks/${this.cookbookName}/${this.currentVersion}/file-content?url=${this.contentUrl}`)
      .pipe(first())
      .subscribe
      (fileContent => {
        this.urlContent = fileContent;
        this.contentTabLoading = false;
        this.contentLoading = false;
        this.contentData = this.urlContent.content.toString();
      });
  }

  onSelectedTab(event: { target: { value: CookbookDetailsTab } }) {
    this.tabValue = event.target.value;
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }
}
