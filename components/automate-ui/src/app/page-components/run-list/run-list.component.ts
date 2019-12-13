import {
  Component,
  ChangeDetectionStrategy,
  Input,
  OnChanges,
  SimpleChanges,
  ChangeDetectorRef
} from '@angular/core';
import { NodeRunsService } from '../../services/node-details/node-runs.service';
import { NodeRun, PolicyCookbooks, ExpandedRunListItem, Resource, VersionedCookbook } from '../../types/types';

export class Item {
  type: string;
  name: string;
  version: string;
  skipped: boolean;
  children: Item[];
  has_child_role: boolean;
  status: string;
  policy_identifier: string;

  constructor(item: ExpandedRunListItem) {
    this.type = item.type;
    this.name = item.name;
    this.version = item.version;
    this.skipped = item.skipped;
    this.children = [];
  }

  isRole(): boolean {
    return this.type === 'role';
  }

  cookbookName(): string {
    return this.name.split('::')[0];
  }
}

class RunListRollupStatus {
  cookbooks = new Set<string>();
  roles = new Set<string>();
  recipes = new Set<string>();
}

@Component({
  selector: 'app-run-list',
  templateUrl: './run-list.component.html',
  styleUrls: ['./run-list.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush
})
export class RunListComponent implements OnChanges {
  @Input() nodeRun: NodeRun;

  runList: Item[] = [];
  cookbooksCount = 0;
  rolesCount = 0;
  recipesCount = 0;
  failedCount = 0;
  successCount = 0;

  constructor(private nodeRunsService: NodeRunsService,
    private cdr: ChangeDetectorRef) {}

  ngOnChanges(changes: SimpleChanges): void {
    if (changes['nodeRun']) {
      const runList = this.createRunList(changes['nodeRun'].currentValue);
      const runListRollupStatus = this.collectRunListRollupStatus(runList);
      this.failedCount = this.statusCount(runList, 'failed');
      this.successCount = this.statusCount(runList, 'success');
      this.recipesCount = runListRollupStatus.recipes.size;
      this.cookbooksCount = runListRollupStatus.cookbooks.size;
      this.rolesCount = runListRollupStatus.roles.size;
      this.runList = runList;
    }
  }

  private createRunList(newNodeRun: NodeRun): Item[] {
    let runList: Item[] = [];
    const expandedRunList = newNodeRun.expandedRunList;
    const resources = newNodeRun.resources || [];
    const versionedCookbooks = newNodeRun.versionedCookbooks || [];
    if (expandedRunList) {
      runList = expandedRunList.run_list.map((item) =>
        this.createItem(item, resources, versionedCookbooks));
      this.maybeFetchPolicy(runList, newNodeRun.policyRevision);
    }

    return runList;
  }

  private maybeFetchPolicy(runList: Item[], policyRevision: string): void {
    if (policyRevision) {
      this.nodeRunsService.getPolicyCookbooks(policyRevision).then(policyCookbooks => {
        this.appendIdentifierToRecipe(runList, policyCookbooks);
        this.cdr.markForCheck();
      });
    }
  }

  private createItem(originalItem: ExpandedRunListItem, resources: Resource[],
    versionedCookbooks: VersionedCookbook[]): Item {
    const item = new Item(originalItem);

    if (item.isRole()) {
      item.has_child_role = false;
      originalItem.children.forEach(child => {
        if (child.type === 'role') {
          item.has_child_role = true;
        }
      });
      item.children = originalItem.children.map((childItem) =>
        this.createItem(childItem, resources, versionedCookbooks));
    } else {
      const cookbookName = item.cookbookName();
      item.status = 'success';
      item.version = this.extractVersionFromCookbook(cookbookName, versionedCookbooks);

      resources.forEach(resource => {
        if (resource.cookbook_name === cookbookName && resource.status === 'failed') {
            item.status = 'failed';
        }
      });
    }

    return item;
  }

  private appendIdentifierToRecipe(runList: Item[], policyResults: PolicyCookbooks): void {
    const cookbookLocks = policyResults.cookbookLocks;

    runList.forEach((item) => {
      if (!item.isRole()) {
        const cookbook = item.cookbookName();
        item.policy_identifier = cookbookLocks[cookbook];
      }
    });
  }

  private extractVersionFromCookbook(name: string,
    versionedCookbooks: VersionedCookbook[]): string {
    const cookbook = versionedCookbooks.find(c => c.name === name);
    let version = '';

    if (cookbook) {
      version = cookbook.version;
    }

    return version;
  }

  private collectRunListRollupStatus(items: Item[]): RunListRollupStatus {
    const runListData = new RunListRollupStatus();

    items.forEach((item) => {
      if (item.isRole()) {
        runListData.roles.add(item.name);
        const childRunListData = this.collectRunListRollupStatus(item.children);

        childRunListData.recipes.forEach(runListData.recipes.add, runListData.recipes);
        childRunListData.cookbooks.forEach(runListData.cookbooks.add, runListData.cookbooks);
        childRunListData.roles.forEach(runListData.roles.add, runListData.roles);
      } else {
        const cookbook = item.cookbookName();
        runListData.cookbooks.add(cookbook);
        runListData.recipes.add(item.name);
      }
    });

    return runListData;
  }

  private statusCount(items: Item[], status: string): number {
    let count = 0;

    items.forEach((item) => {
      if (item.status && item.status === status) {
        count++;
      } else if (item.children) {
        count += this.statusCount(item.children, status);
      }
    });

    return count;
  }
}
