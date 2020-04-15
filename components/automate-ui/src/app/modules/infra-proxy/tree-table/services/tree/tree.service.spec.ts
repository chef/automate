import { TestBed } from '@angular/core/testing';
import { TreeService } from './tree.service';
import * as _ from 'lodash';
import { mockSearchableTree } from '../../mocks/mockSearchableTree';
import { ChildLists } from '../../mocks/models';
import { some, none } from 'fp-ts/lib/Option';
import { SearchableNode } from '../../models';

describe('TreeService', () => {
  beforeEach(() => TestBed.configureTestingModule({}));

  it('should be created', () => {
    const service: TreeService = TestBed.inject(TreeService);
    expect(service).toBeTruthy();
  });

  it('should search for a node in a tree and return it warapped in Option<> if present', () => {
    const service: TreeService = TestBed.inject(TreeService);
    const tree = _.cloneDeep(mockSearchableTree);
    const expectedNode = tree.children[0];
    const expectedPathToRoot = [tree];
    const id = expectedNode.id;
    expect(service.searchById(tree, id)).toEqual(some({
      ...expectedNode,
      pathToRoot: expectedPathToRoot
    }));
  });

  it('should search for a node in a tree and return none if not present', () => {
    const service: TreeService = TestBed.inject(TreeService);
    const tree = _.cloneDeep(mockSearchableTree);
    const id = '00000';
    expect(service.searchById(tree, id)).toEqual(none);
  });

  it('should correctly flatten a tree', () => {
    const service: TreeService = TestBed.inject(TreeService);
    const tree = _.cloneDeep(mockSearchableTree);
    const expectedFlattenedTree = [
      tree,
      tree.children[0],
      tree.children[1],
      tree.children[2],
      tree.children[2].children[0],
      tree.children[2].children[1],
      tree.children[2].children[1].children[0],
      tree.children[2].children[1].children[1]
    ];
    expect(service.flatten(tree)).toEqual(expectedFlattenedTree);
  });

  it('should return the depth of a node that\'s in the tree', () => {
    const service: TreeService = TestBed.inject(TreeService);
    const tree = _.cloneDeep(mockSearchableTree);
    const firstLevelNode = tree.children[0];
    const secondLevelNode = tree.children[2].children[0];
    const thirdLevelNode = tree.children[2].children[1].children[0];
    expect(service.getNodeDepth(tree, tree)).toEqual(0);
    expect(service.getNodeDepth(tree, firstLevelNode)).toEqual(1);
    expect(service.getNodeDepth(tree, secondLevelNode)).toEqual(2);
    expect(service.getNodeDepth(tree, thirdLevelNode)).toEqual(3);
  });

  it('should return a depth of -1 when the node is not in the tree', () => {
    const service: TreeService = TestBed.inject(TreeService);
    const tree = _.cloneDeep(mockSearchableTree);
    const node: SearchableNode<ChildLists> = {
      id: '0000',
      value: {
        name: 'name',
        version: '3.0',
        type: 'role'
      },
      children: []
    };
    expect(service.getNodeDepth(tree, node)).toEqual(-1);
  });
});
