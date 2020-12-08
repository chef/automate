import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { ChefStatusIconPipe } from '../../pipes/chef-status-icon.pipe';
import { NodeRollupComponent } from './node-rollup.component';

describe('NodeRollupComponent', () => {
  let component: NodeRollupComponent;
  let fixture: ComponentFixture<NodeRollupComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        ChefStatusIconPipe,
        NodeRollupComponent
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(NodeRollupComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});
