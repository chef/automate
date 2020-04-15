export interface Node<T> {
  value: T;
  children: Node<T>[];
}

export interface SearchableNode<T> extends Node<T> {
  id: string;
  children: SearchableNode<T>[];
}

export interface TreeTableNode<T> extends SearchableNode<T> {
  depth: number;
  isVisible: boolean;
  isExpanded: boolean;
  children: TreeTableNode<T>[];
}

export interface NodeInTree<T> extends SearchableNode<T> {
  pathToRoot: SearchableNode<T>[];
}

export interface Options<T> {
  verticalSeparator?: boolean;
  capitalisedHeader?: boolean;
  highlightRowOnHover?: boolean;
  customColumnOrder?: Array<keyof T> & string[];
  elevation?: number;
}
