import { Component, isDevMode, OnInit } from '@angular/core';
import { Store } from '@ngrx/store';
import { Observable } from 'rxjs';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { isIAMv2 } from 'app/entities/policies/policy.selectors';

@Component({
  selector: 'app-navbar',
  templateUrl: './navbar.component.html',
  styleUrls: ['./navbar.component.scss']
})

export class NavbarComponent implements OnInit {
  public isIAMv2$: Observable<boolean>;

  constructor(
    private store: Store<NgrxStateAtom>
  ) {}

  isDevMode() {
    return isDevMode();
  }

  ngOnInit() {
    this.isIAMv2$ = this.store.select(isIAMv2);
  }
}
