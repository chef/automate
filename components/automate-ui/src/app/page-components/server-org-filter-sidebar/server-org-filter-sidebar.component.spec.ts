import { async, TestBed } from '@angular/core/testing';
import { ServerOrgFilterSidebarComponent } from './server-org-filter-sidebar.component';
import { RouterTestingModule } from '@angular/router/testing';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { CommonModule } from '@angular/common';
import { StoreModule, Store } from '@ngrx/store';
import * as sidebar from '../../services/sidebar/sidebar.reducer';
import * as sidebarActions from '../../services/sidebar/sidebar.actions';
import * as sidebarSelectors from '../../services/sidebar/sidebar.selectors';

describe('ServerOrgFilterSidebarComponent', () => {
  let fixture, component;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        CommonModule,
        StoreModule.forRoot({
          sidebar: sidebar.sidebarReducer
        })
      ],
      declarations: [
        ServerOrgFilterSidebarComponent
      ],
      schemas: [CUSTOM_ELEMENTS_SCHEMA]
    }).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ServerOrgFilterSidebarComponent);
    component = fixture.componentInstance;
    this.ngrxStore = TestBed.get(Store);
    component.ngOnInit();
    this.ngrxStore.dispatch(
      sidebarActions.getChefServersSuccess(['source1', 'source2', 'localhost']));

    this.ngrxStore.dispatch(sidebarActions.getOrgsSuccess(['org1', 'org2']));
  });

  it('converts incoming localhost chef server to Local Mode', () => {
    component.servers.subscribe(servers =>
      expect(servers).toEqual(['source1', 'source2', component.localModeTag])
    );
    component.organizations.subscribe(organizations =>
      expect(organizations).toEqual(['org1', 'org2'])
    );
  });

  it('When filter is set with Local Mode, localhost is set instead', () => {
    this.selectedChefServers = [];
    this.ngrxStore.select(sidebarSelectors.selectedChefServers)
    .subscribe(updatedSelectedChefServers =>
      this.selectedChefServers = updatedSelectedChefServers
    );

    component.filter('servers', [component.localModeTag]);

    expect(this.selectedChefServers).toEqual([component.localModeSource]);
  });

  it('When filter is set with Local Mode and other server name, ' +
    'localhost is only swapped for Local Mode', () => {
    this.selectedChefServers = [];
    this.ngrxStore.select(sidebarSelectors.selectedChefServers)
      .subscribe(updateSelectedChefServers =>
        this.selectedChefServers = updateSelectedChefServers);

    component.filter('servers', [component.localModeTag, 'www.chef.io']);

    expect(this.selectedChefServers).toEqual([component.localModeSource, 'www.chef.io']);
  });
});
