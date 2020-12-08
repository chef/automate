import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { TreeTableComponent } from './tree-table.component';
import { MatTableModule } from '@angular/material/table';
import { MatIconModule } from '@angular/material/icon';
import { mockSearchableTree } from '../mocks/mockSearchableTree';
import * as _ from 'lodash/fp';

describe('TreeTableComponent', () => {
  let component: TreeTableComponent<any>;
  let fixture: ComponentFixture<TreeTableComponent<any>>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        TreeTableComponent
      ],
      imports: [
        MatTableModule,
        MatIconModule
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TreeTableComponent);
    component = fixture.componentInstance;
    component.tree = _.cloneDeep(mockSearchableTree);
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should emit an event when a node is clicked', () => {
    const clickedNode = (component as any).treeTable[0];
    component.nodeClicked.subscribe(n => expect(n).toBe(clickedNode));
    component.onNodeClick(clickedNode);
  });

});
