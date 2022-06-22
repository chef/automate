import {
  Component,
  OnInit,
  Input,
  Output,
  OnChanges,
  SimpleChanges
} from '@angular/core';
import { Node, TreeTableNode, Options, SearchableNode } from '../models';
import { TreeService } from '../services/tree/tree.service';
import { MatTableDataSource } from '@angular/material/table';
import { ValidatorService } from '../services/validator/validator.service';
import { ConverterService } from '../services/converter/converter.service';
import { defaultOptions } from '../default.options';
import { flatMap, defaults } from 'lodash';
import { Subject } from 'rxjs';
import * as O from 'fp-ts/lib/Option';
import { pipe } from 'fp-ts/lib/pipeable';

@Component({
  selector: 'app-tree-table',
  templateUrl: './tree-table.component.html',
  styleUrls: ['./tree-table.component.scss']
})
export class TreeTableComponent<T> implements OnInit, OnChanges {
  @Input() tree: Node<T> | Node<T>[];
  @Input() options: Options<T> = {};
  @Output() nodeClicked: Subject<TreeTableNode<T>> = new Subject();

  displayedColumns: string[];
  dataSource: MatTableDataSource<TreeTableNode<T>>;
  private searchableTree: SearchableNode<T>[];
  private treeTable: TreeTableNode<T>[];

  constructor(
    private treeService: TreeService,
    private validatorService: ValidatorService,
    private converterService: ConverterService
  ) { }

  ngOnInit() {
    this.tree = Array.isArray(this.tree) ? this.tree : [this.tree];
    this.options = this.parseOptions(defaultOptions);
    if (this.tree.length > 0) {
      const customOrderValidator =
      this.validatorService.validateCustomOrder(this.tree[0], this.options.customColumnOrder);
      if (this.options.customColumnOrder && !customOrderValidator.valid) {
        throw new Error(
          `Properties ${customOrderValidator.xor.map(x => `'${x}'`).join(', ')} incorrect or missing in customColumnOrder`
        );
      }
      this.displayedColumns = this.options.customColumnOrder
        ? this.options.customColumnOrder
        : this.extractNodeProps(this.tree[0]);
      const filterColumns = ['type', 'no_version', 'error', 'skipped'];
      this.displayedColumns = this.displayedColumns.filter(v => !filterColumns.includes(v));
      this.searchableTree = this.tree.map(t => this.converterService.toSearchableTree(t));
      const treeTableTree =
        this.searchableTree.map(st => this.converterService.toTreeTableTree(st));
      this.treeTable = flatMap(treeTableTree, this.treeService.flatten);
      this.treeCollapsed();
      this.dataSource = this.generateDataSource();
    }
  }

  ngOnChanges(changes: SimpleChanges) {
    if (changes.tree.isFirstChange()) {
      return;
    }
    this.tree = Array.isArray(this.tree) ? this.tree : [this.tree];
    this.searchableTree = this.tree.map(t => this.converterService.toSearchableTree(t));
    const treeTableTree = this.searchableTree.map(st => this.converterService.toTreeTableTree(st));
    this.treeTable = flatMap(treeTableTree, this.treeService.flatten);
    this.treeCollapsed();
    this.dataSource = this.generateDataSource();
  }

  extractNodeProps(tree: Node<T> & { value: { [k: string]: any } }): string[] {
    return Object.keys(tree.value).filter(x => typeof tree.value[x] !== 'object');
  }

  generateDataSource(): MatTableDataSource<TreeTableNode<T>> {
    return new MatTableDataSource(this.treeTable.filter(x => x.isVisible));
  }

  // A given element `el` is marked visible if every node between it and the root is expanded.
  onNodeClick(clickedNode: TreeTableNode<T>): void {
    clickedNode.isExpanded = !clickedNode.isExpanded;
    this.treeTable.forEach(el => {
      el.isVisible = this.searchableTree.every(st => {
        return pipe(this.treeService.searchById(st, el.id),
        O.fold(() => [], n => n.pathToRoot)).
        every(p => this.treeTable.find(x => x.id === p.id).isExpanded);
      });
    });
    this.dataSource = this.generateDataSource();
    this.nodeClicked.next(clickedNode);
  }

  treeCollapsed() {
    this.treeTable.forEach((item, index) => {
      item.isExpanded = false;
      if (item.depth > 0 && index > 0) {
        item.isVisible = false;
      }
    });
    this.dataSource = this.generateDataSource();
  }

  expand() {
    this.treeTable.forEach((item, index) => {
      item.isExpanded = true;
      if (item.depth > 0 && index > 0) {
        item.isVisible = true;
      }
    });
    this.dataSource = this.generateDataSource();
  }

  // Overrides default options with those specified by the user
  parseOptions(defaultOpts: Options<T>): Options<T> {
    return defaults(this.options, defaultOpts);
  }
}
