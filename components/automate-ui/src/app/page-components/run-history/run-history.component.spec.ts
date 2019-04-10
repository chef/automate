import { TestBed, ComponentFixture } from '@angular/core/testing';
import { RunHistoryComponent } from '../run-history/run-history.component';
import { RunHistoryStore } from '../../services/run-history-store/run-history.store';
import { DateSelectorComponent } from '../date-selector/date-selector.component';
import { StatusSelectorPipe } from '../../pipes/status-selector.pipe';
import { ChefStatusIconPipe } from '../../pipes/chef-status-icon.pipe';
import { HistorySelection } from '../../helpers/history-selection/history-selection';
import { AbridgedNodeRun, NodeHistoryCountsFilter, NodeRunsCount } from '../../types/types';
import { NodeRunsService } from '../../services/node-details/node-runs.service';
import { NodeHistoryFilter } from '../../types/types';
import { MockComponent } from 'ng2-mock-component';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';

describe('RunHistoryComponent', () => {
  let fixture: ComponentFixture<RunHistoryComponent>;
  let component: RunHistoryComponent;

  class MockNodeRunsService {
    getNodeRuns(_filter: NodeHistoryFilter): Promise<AbridgedNodeRun[]> {
      return Promise.resolve([]);
    }
    getNodeRunCounts(_filter: NodeHistoryCountsFilter): Promise<NodeRunsCount> {
      return Promise.resolve(new NodeRunsCount({total: 0, success: 0, failure: 0}));
    }
  }

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [
        RunHistoryComponent,
        DateSelectorComponent,
        StatusSelectorPipe,
        ChefStatusIconPipe,
        MockComponent({selector: 'chef-icon'})
      ],
      providers: [
        RunHistoryStore,
        { provide: NodeRunsService, useClass: MockNodeRunsService }
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });

    fixture = TestBed.createComponent(RunHistoryComponent);
    component = fixture.componentInstance;
  });

  describe('when a date range is selected', () => {
    it('calculates the date range', () => {
      spyOn(HistorySelection, 'startingTimestamp');
      component.dateSelected('Last month');
      expect(HistorySelection.startingTimestamp).toHaveBeenCalledWith('Last month');
    });
  });

  describe('defaultSelectionTerm when the component loads', () => {
    beforeEach(() => {
      component.nodeId = '123';
      component.initialRunId = 'initialRunId';
    });

    it('is set to last 24 hours if initial is within 24 hours', () => {
      component.initialDate = new Date();
      fixture.detectChanges();
      expect(component.defaultSelectionTerm).toEqual('Last 24 hours');
    });

    it('is set to Last month if initial is between a week and a month ago', () => {
      component.initialDate = new Date();
      component.initialDate.setDate(component.initialDate.getDate() - 8);
      fixture.detectChanges();
      expect(component.defaultSelectionTerm).toEqual('Last month');
    });

    it('is set to Last week if initial is greater than 24 hours, but a week of less', () => {
      component.initialDate = new Date();
      component.initialDate.setDate(component.initialDate.getDate() - 5);
      fixture.detectChanges();
      expect(component.defaultSelectionTerm).toEqual('Last week');
    });

    it('is set to Last 3 months if initial is greater than a month', () => {
      component.initialDate = new Date();
      component.initialDate.setDate(component.initialDate.getDate() - 40);
      fixture.detectChanges();
      expect(component.defaultSelectionTerm).toEqual('Last 3 months');
    });

    it('is set to Last 24 hours if initial is greater than 3 months', () => {
      component.initialDate = new Date();
      component.initialDate.setDate(component.initialDate.getDate() - 120);
      fixture.detectChanges();
      expect(component.defaultSelectionTerm).toEqual('Last 24 hours');
    });

    it('is set to Last 24 hours if not initialDate', () => {
      component.initialDate = null;
      fixture.detectChanges();
      expect(component.defaultSelectionTerm).toEqual('Last 24 hours');
    });
  });
});
