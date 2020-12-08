import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { using } from 'app/testing/spec-helpers';
import { ConfirmApplyStopModalComponent } from './confirm-apply-stop-modal.component';
import { ApplyRulesStatus } from 'app/entities/projects/project.reducer';

describe('ConfirmApplyStopModalComponent', () => {
  let component: ConfirmApplyStopModalComponent;
  let fixture: ComponentFixture<ConfirmApplyStopModalComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ],
      declarations: [
        ConfirmApplyStopModalComponent
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ConfirmApplyStopModalComponent);
    component = fixture.componentInstance;
    component.applyRulesStatus = {
      state: 'running',
      estimatedTimeComplete: '0001-01-01T00:00:00Z',
      percentageComplete: 0.5,
      failed: false,
      failureMessage: ''
    } as ApplyRulesStatus;
    fixture.detectChanges();
  });

  it('renders', () => {
    const modal = fixture.nativeElement;
    expect(modal).not.toBeNull();
  });

  it('displays a title', () => {
    const title = fixture.nativeElement.querySelector('#title');
    expect(title).not.toBeNull();
    expect(title.textContent.trim()).toEqual('Stop Updating Projects?');
  });

  it('displays a progress bar', () => {
    const progressBar = fixture.nativeElement.querySelector('chef-progress-bar');
    expect(progressBar).not.toBeNull();
  });

  describe('progress bar', () => {
    using([
      [0,    'Preparing...'],
      [0.75, 'Preparing...'],
      [10,   '10% complete'],
      [91.9, '91% complete'],
      [92,   '92% complete'],
      [100,  'Finishing Up...']
    ], (percentageComplete, progressPrefixText) => {
      it('displays correct percentage', () => {
        component.updateProgress({ ...component.applyRulesStatus, percentageComplete });
        expect(component.progressValue).toEqual(percentageComplete);
      });

      it('displays correct prefix text', () => {
        component.updateProgress({ ...component.applyRulesStatus, percentageComplete });
        expect(component.progressPrefixText).toEqual(progressPrefixText);
      });
    });

    const NOW = '2019-01-01T00:00:00Z';

    using([
      // floor(0) percent and ceil(100) percent don't have time estimates
      [0, '0001-01-01T00:00:00Z', ''],
      [0, '1970-01-01T00:00:00Z', ''],
      [0.5, '2019-01-01T00:00:00Z', ''],
      [10, '2019-01-01T00:15:00Z', '00:15:00 until finished'],
      [50, '2019-01-01T12:30:45Z', '12:30:45 until finished'],
      [99.9, '2019-01-02T06:45:15Z', '']
    ], (percentageComplete, estimatedTimeComplete, progressSuffixText) => {
      beforeEach(() => jasmine.clock().mockDate(new Date(NOW)));

      afterEach(() => jasmine.clock().uninstall());

      it('displays correct suffix text', () => {
        component.updateProgress({
          ...component.applyRulesStatus,
          percentageComplete,
          estimatedTimeComplete
        });
        expect(component.progressValue).toEqual(percentageComplete);
        expect(component.progressSuffixText).toEqual(progressSuffixText);
      });
    });

    it('does not update percentage once update is cancelled', () => {
      component.updateProgress({ ...component.applyRulesStatus, percentageComplete: 10.0201 });
      expect(component.progressPrefixText).toContain('10%');
      component.updateProgress({ ...component.applyRulesStatus, percentageComplete: 20.5 });
      expect(component.progressPrefixText).toContain('20%');

      component.stopRulesInProgress = true;

      component.updateProgress({ ...component.applyRulesStatus, percentageComplete: 30.111 });
      expect(component.progressPrefixText).toContain('20%');
    });

    it('does not update time once update is cancelled', () => {
      const percentageComplete = 30;
      jasmine.clock().mockDate(new Date(NOW));
      component.updateProgress(
        { ...component.applyRulesStatus, percentageComplete, estimatedTimeComplete: '2019-01-01T12:30:45Z' });
      expect(component.progressSuffixText).toContain('12:30:45');
      component.updateProgress(
        { ...component.applyRulesStatus, percentageComplete, estimatedTimeComplete: '2019-01-01T11:30:45Z' });
      expect(component.progressSuffixText).toContain('11:30:45');

      component.stopRulesInProgress = true;

      component.updateProgress(
        { ...component.applyRulesStatus, percentageComplete, estimatedTimeComplete: '2019-01-01T09:30:45Z' });
      expect(component.progressSuffixText).toContain('11:30:45');
    });
  });

  using([
    [true, 'Stopping Project Update...', 'in progress'],
    [false, 'Stop Updating Projects', 'not in progress']
  ], (inProgress: boolean, buttonText: string, description: string) => {
    it(`displays a confirm-button when cancel is ${description}`, () => {
      component.stopRulesInProgress = inProgress;
      fixture.detectChanges();
      const button = fixture.nativeElement.querySelector('#confirm-button');
      expect(button).not.toBeNull();
      expect(button.textContent.trim()).toEqual(buttonText);
    });
  });

  describe('when confirm-button is clicked', () => {
    it('emits a confirm event', waitForAsync () => {
      spyOn(component.confirm, 'emit');

      const button = fixture.nativeElement.querySelector('#confirm-button');
      await button.click();

      expect(component.confirm.emit).toHaveBeenCalled();
    });
  });

  it('displays a cancel-button', () => {
    const button = fixture.nativeElement.querySelector('#cancel-button');
    expect(button).not.toBeNull();
    expect(button.textContent.trim()).toEqual('Cancel');
  });

  describe('when cancel-button is clicked', () => {
    it('emits a cancel event', waitForAsync () => {
      spyOn(component.cancel, 'emit');

      const button = fixture.nativeElement.querySelector('#cancel-button');
      await button.click();

      expect(component.cancel.emit).toHaveBeenCalled();
    });
  });
});
