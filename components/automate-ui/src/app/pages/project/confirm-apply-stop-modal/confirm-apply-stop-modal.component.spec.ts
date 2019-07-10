import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
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
