import { startWith, takeUntil } from 'rxjs/operators';
import { Component, Input, OnInit, OnDestroy } from '@angular/core';
import { Subject } from 'rxjs';
import { FormGroup } from '@angular/forms';

@Component({
  selector: 'chef-job-profiles-form',
  templateUrl: './job-profiles-form.component.html',
  styleUrls: ['./job-profiles-form.component.scss']
})
export class JobProfilesFormComponent implements OnInit, OnDestroy {
  @Input() form: FormGroup;

  private isDestroyed: Subject<boolean> = new Subject();

  ngOnInit() {
    this.form.get('allSelected').valueChanges.pipe(
      takeUntil(this.isDestroyed))
      .subscribe(allSelected => {
        const someSelectedControl = this.form.get('someSelected');
        const profileControls =  this.form.get('profiles')['controls'];
        someSelectedControl.setValue(false, { emitEvent: false });
        profileControls.forEach(c => c.get('include').setValue(allSelected, { emitEvent: false }));
      });

    this.form.get('profiles').valueChanges.pipe(
      startWith(this.form.get('profiles').value),
      takeUntil(this.isDestroyed))
      .subscribe(profiles => {
        const allSelected = profiles.every(p => p.include);
        const someSelected = profiles.some(p => p.include) && !allSelected;
        this.form.get('allSelected').setValue(allSelected, { emitEvent: false });
        this.form.get('someSelected').setValue(someSelected, { emitEvent: false });
      });
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }
}
