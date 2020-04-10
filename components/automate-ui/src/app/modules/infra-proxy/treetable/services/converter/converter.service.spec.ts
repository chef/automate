import { TestBed } from '@angular/core/testing';
import { ConverterService } from './converter.service';
import { SearchableNode, TreeTableNode } from '../../models';
import { mockSearchableTree } from '../../mocks/mockSearchableTree';
import { mockTree } from '../../mocks/mockTree';
import { mockTreeTableTree } from '../../mocks/mockTreeTableTree';
import * as _ from 'lodash';

describe('ConverterService', () => {
  beforeEach(() => TestBed.configureTestingModule({}));

  it('should be created', () => {
    const service: ConverterService = TestBed.inject(ConverterService);
    expect(service).toBeTruthy();
  });

  it('should convert a basic tree into a searchable tree', () => {
    const service: ConverterService = TestBed.inject(ConverterService);
    expect(instanceOfSearchableNode(mockTree)).toBe(false);
    expect(instanceOfSearchableNode(service.toSearchableTree(mockTree))).toBe(true);
  });

  it('should convert a searchable tree into a treetable tree', () => {
    const service: ConverterService = TestBed.inject(ConverterService);
    expect(instanceOfSearchableNode(mockSearchableTree)).toBe(true);
    expect(instanceOfTreeTableNodee(mockSearchableTree)).toBe(false);
    expect(instanceOfSearchableNode(service.toTreeTableTree(mockSearchableTree))).toBe(true);
  });

  it('should do nothing to trees that are already of the required type', () => {
    const service: ConverterService = TestBed.inject(ConverterService);
    expect(instanceOfSearchableNode(mockSearchableTree)).toBe(true);
    expect(instanceOfSearchableNode(service.toSearchableTree(mockSearchableTree))).toBe(true);
  });

  it('should clone the trees', () => {
    const service: ConverterService = TestBed.inject(ConverterService);
    expect(mockSearchableTree !== service.toSearchableTree(mockSearchableTree)).toBe(true);
    expect(_.isEqual(mockSearchableTree, service.toSearchableTree(mockSearchableTree))).toBe(true);
    expect(mockTreeTableTree !== service.toTreeTableTree(mockTreeTableTree)).toBe(true);
    expect(_.isEqual(mockTreeTableTree, service.toTreeTableTree(mockTreeTableTree))).toBe(true);
  });

});

export function instanceOfSearchableNode<T>(object: any): object is SearchableNode<T> {
  return 'id' in object;
}

export function instanceOfTreeTableNodee<T>(object: any): object is TreeTableNode<T> {
  return 'depth' in object && 'isVisible' in object && 'isExpanded' in object;
}
