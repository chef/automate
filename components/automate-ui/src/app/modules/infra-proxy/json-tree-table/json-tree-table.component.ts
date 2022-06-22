import { Component, Input, ElementRef, OnChanges } from '@angular/core';
import * as jsonTree from 'app/page-components/json-tree/vendor/json-tree';

@Component({
  selector: 'app-json-tree-table',
  template: '<div id="tree-container" class="scroll"></div>',
  styleUrls: ['./json-tree-table.component.scss']
})
export class JsonTreeTableComponent implements OnChanges {

  @Input() json: Object;
  tree: any;

  constructor(private el: ElementRef) {}

  ngOnChanges() {
    this.build();
  }

  expand() {
    if (this.tree) {
      this.tree.expand();
    }
  }

  collapse() {
    if (this.tree) {
      this.tree.collapse();
    }
  }

  search(term: string): number {
    let resultCount = 0;
    this.reset();

    function parentsOf(node) {
      const parents = [];
      let p = node.parentNode;
      while (p) {
        parents.push(p);
        p = p.parentNode;
      }
      return parents;
    }

    if (term) {
      const nodes = this.el.nativeElement.querySelectorAll(`[data-value*="${term.toLowerCase()}"]`);
      for (const node of nodes) {
        node.classList.add('highlight');
        parentsOf(node).forEach(p => {
          if (p.classList && p.classList.contains('jsontree_node_complex')) {
            p.classList.add('jsontree_node_expanded');
          }
        });
      }
      resultCount = nodes.length;
    }
    return resultCount;
  }

  reset() {
    this.collapse();
    this.unhighlight();
  }

  unhighlight() {
    for (const node of this.el.nativeElement.querySelectorAll('.highlight')) {
      node.classList.remove('highlight');
    }
  }

  private build() {
    if (this.json) {
      if (!this.tree) {
        const e = this.el.nativeElement.querySelector('#tree-container');
        this.tree = jsonTree.create(this.json, e);
      } else {
        this.tree.loadData(this.json);
      }
      const el = this.el.nativeElement;
      if (el.querySelector('ul.jsontree_tree li')) {
        ['span.jsontree_label', 'span.jsontree_value'].forEach(s => {
          for (const node of el.querySelectorAll(s)) {
            node.setAttribute('data-value', dequote(node.innerText).toLowerCase());
          }
        });
      }
    }

    function dequote(s: string) {
      return s.replace(/"/g, '');
    }
  }
}
