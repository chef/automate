import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { using } from 'app/testing/spec-helpers';
import { ConfirmApplyStopModalComponent } from './confirm-apply-stop-modal.component';
import { ApplyRulesStatus } from 'app/entities/projects/project.reducer';

describe('ConfirmApplyStopModalComponent', () => {
  let component: ConfirmApplyStopModalComponent;
  let fixture: ComponentFixture<ConfirmApplyStopModalComponent>;

  beforeEach(async(() => {
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
      [0,     '0% complete'],
      [0.1,   '10% complete'],
      [0.5,   '50% complete'],
      [0.919, '91% complete'],
      [0.92,  '92% complete'],
      [1,     '100% complete']
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
      ['0001-01-01T00:00:00Z', '00:00:00 until finished'],
      ['1970-01-01T00:00:00Z', '00:00:00 until finished'],
      ['2019-01-01T00:00:00Z', '00:00:00 until finished'],
      ['2019-01-01T00:15:00Z', '00:15:00 until finished'],
      ['2019-01-01T12:30:45Z', '12:30:45 until finished'],
      ['2019-01-02T06:45:15Z', '30:45:15 until finished']
    ], (estimatedTimeComplete, progressSuffixText) => {
      beforeEach(() => jasmine.clock().mockDate(new Date(NOW)));

      afterEach(() => jasmine.clock().uninstall());

      it('displays correct suffix text', () => {
        component.updateProgress({ ...component.applyRulesStatus, estimatedTimeComplete });
        expect(component.progressSuffixText).toEqual(progressSuffixText);
      });
    });
  });

  it('displays a confirm-button', () => {
    const button = fixture.nativeElement.querySelector('#confirm-button');
    expect(button).not.toBeNull();
    expect(button.textContent.trim()).toEqual('Stop Updating Projects');
  });

  describe('when confirm-button is clicked', () => {
    it('emits a confirm event', async () => {
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
    it('emits a cancel event', async () => {
      spyOn(component.cancel, 'emit');

      const button = fixture.nativeElement.querySelector('#cancel-button');
      await button.click();

      expect(component.cancel.emit).toHaveBeenCalled();
    });
  });
});
