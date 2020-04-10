import { Injectable } from '@angular/core';
import { TreeService } from '../tree/tree.service';
import { Node, SearchableNode, TreeTableNode } from '../../models';
import { cloneDeep } from 'lodash';
import { v4 as uuidv4 } from 'uuid';

@Injectable({
  providedIn: 'root'
})
export class ConverterService {
  constructor(private treeService: TreeService) { }

  /**
   * Clone a Node<T> object and convert it to a SearchableNode<T>
   * @param tree the node to be converted
  */
  toSearchableTree<T>(tree: Node<T>): SearchableNode<T> {
    const treeClone = cloneDeep(tree) as SearchableNode<T>;
    this.treeService.traverse(treeClone, (node: SearchableNode<T>) => {
      node.id = node.id ? node.id : uuidv4();
    });
    return treeClone;
  }

  /**
   * Clone a SearchableNode<T> object and convert it to a TreeTableNode<T>
   * @param tree the node to be converted
  */
  toTreeTableTree<T>(tree: SearchableNode<T>): TreeTableNode<T> {
    const treeClone = cloneDeep(tree) as TreeTableNode<T>;
    this.treeService.traverse(treeClone, (node: TreeTableNode<T>) => {
      node.depth = this.treeService.getNodeDepth(treeClone, node);
      node.isExpanded = true;
      node.isVisible = true;
    });
    return treeClone;
  }
}
