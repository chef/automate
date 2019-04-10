import { Set } from 'immutable';
import {
  Component,
  Input,
  Output,
  OnChanges,
  EventEmitter,
  SimpleChanges } from '@angular/core';
  import {
    Node
  } from '../../entities/client-runs/client-runs.model';

@Component({
  selector: 'app-deletable-node-control',
  templateUrl: './deletable-node-control.component.html',
  styleUrls: ['./deletable-node-control.component.scss']
})

export class DeletableNodeControlComponent implements OnChanges {
  @Input() deletableNodes: Node[] = [];
  @Input() isVisible = false;
  @Output() deleteNodes: EventEmitter<any> = new EventEmitter<any>();
  deleteConfirmModalVisible = false;
  nodesSelectedForDelete = Set<Node>();
  pluralMapping = {
    '=0': 's',
    '=1': '',
    'many': 's',
    'other': 's'
  };

  constructor() {}

  ngOnChanges(changes: SimpleChanges): void {
    if (changes['deletableNodes']) {
      this.deletableNodes = changes['deletableNodes'].currentValue;
      this.nodesSelectedForDelete = this.nodesSelectedForDelete.clear();
    }
  }

  closeModal(isConfirmed: boolean): void {
    if (isConfirmed) {
      const nodeIds = this.nodesSelectedForDelete.toArray().map( node => node.id);
      this.deleteNodes.emit({ nodeIds: nodeIds });
    }
    this.deleteConfirmModalVisible = false;
  }

  updateDeletableNodes(nodes: Node[]): void {
    this.deletableNodes = nodes;
    this.nodesSelectedForDelete = this.nodesSelectedForDelete.clear();
  }

  isAllSelectionOfNodesIndeterminate(): boolean {
    const numberOfNodes = this.numberOfDeletableNodes();
    return numberOfNodes > 0 && !this.nodesSelectedForDelete.isEmpty() &&
      this.nodesSelectedForDelete.size < numberOfNodes;
  }

  showConfModal(): void {
    this.deleteConfirmModalVisible = true;
  }

  selectAllNodesForDelete(isChecked: boolean): void {
    if (isChecked) {
      this.deletableNodes.forEach( (node: Node) =>
        this.nodesSelectedForDelete = this.nodesSelectedForDelete.add(node));
    } else {
      this.nodesSelectedForDelete = this.nodesSelectedForDelete.clear();
    }
  }

  deletableNodesNonEmpty(): boolean {
    return this.numberOfDeletableNodes() > 0;
  }

  numberOfDeletableNodes(): number {
    if ( this.deletableNodes === undefined ) {
      return 0;
    }

    return this.deletableNodes.length;
  }

  areAllNodesSelected(): boolean {
    return this.deletableNodes === undefined ||
      this.deletableNodes.length === this.nodesSelectedForDelete.size;
  }

  isNodeSelected(node: Node): boolean {
    return this.nodesSelectedForDelete.contains(node);
  }

  selectedNodesNonEmpty(): boolean {
    return !this.nodesSelectedForDelete.isEmpty();
  }

  selectedNodesIsEmpty(): boolean {
    return this.nodesSelectedForDelete.isEmpty();
  }

  isNodeDeletable(node: Node): boolean {
    return this.deletableNodes.find( (n: Node) => n === node) !== undefined;
  }

  selectNodeForDelete(isChecked: boolean, node: Node): void {
    if (isChecked) {
      this.nodesSelectedForDelete = this.nodesSelectedForDelete.add(node);
    } else {
      this.nodesSelectedForDelete = this.nodesSelectedForDelete.remove(node);
    }
  }
}
