import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';

import { ClientsComponent } from './clients.component';
import { RouterTestingModule } from '@angular/router/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MockComponent } from 'ng2-mock-component';
import { Store, StoreModule } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { By } from '@angular/platform-browser';
import { GetClientsSuccess } from 'app/entities/clients/client.action';
import { Client } from 'app/entities/clients/client.model';
import { using } from 'app/testing/spec-helpers';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

class MockTelemetryService {
  track() { }
}

describe('ClientsComponent', () => {
  let component: ClientsComponent;
  let fixture: ComponentFixture<ClientsComponent>;
  let element;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'a', inputs: ['routerLink'] }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-heading' }),
        MockComponent({ selector: 'chef-icon' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-page-header' }),
        MockComponent({ selector: 'chef-subheading' }),
        MockComponent({ selector: 'chef-toolbar' }),
        MockComponent({ selector: 'chef-table' }),
        MockComponent({ selector: 'chef-thead' }),
        MockComponent({ selector: 'chef-tbody' }),
        MockComponent({ selector: 'chef-tr' }),
        MockComponent({ selector: 'chef-th' }),
        MockComponent({ selector: 'chef-td' }),
        MockComponent({ selector: 'input', inputs: ['resetOrigin'] }),
        MockComponent({ selector: 'mat-select' }),
        MockComponent({ selector: 'mat-option' }),
        MockComponent({ selector: 'app-create-client-modal', inputs: ['openEvent'] }),
        MockComponent({
          selector: 'app-delete-infra-object-modal',
          inputs: ['default', 'visible', 'objectNoun', 'objectName'],
          outputs: ['close', 'deleteClicked']
        }),
        ClientsComponent
      ],
      providers: [
        { provide: TelemetryService, useClass: MockTelemetryService },
        FeatureFlagsService
      ],
      imports: [
        FormsModule,
        ReactiveFormsModule,
        RouterTestingModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ClientsComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('#search', () => {
    describe('search shows no data', () => {

      using([
        ['contains tilde.', 'client~'],
        ['contains acute, back quote,', 'client`'],
        ['contains exclamation mark', 'client!'],
        ['contains ampersat, at', 'client@'],
        ['contains dollar sign', 'client$'],
        ['contains percent.', 'client%'],
        ['contains caret or circumflex.,', 'client^'],
        ['contains ampersand', 'client&'],
        ['contains asterisk', 'client*'],
        ['contains open or left parenthesis.', 'client('],
        ['contains close or right parenthesis,', 'client)'],
        ['contains plus', 'client+'],
        ['contains equal', 'client='],
        ['contains open brace', 'client{'],
        ['contains close brace', 'client}'],
        ['contains open bracket', 'client['],
        ['contains closed bracket', 'client]'],
        ['contains pipe', 'client|'],
        ['contains backslash', 'client\\'],
        ['contains forward slash', 'client/'],
        ['contains colon', 'client:'],
        ['contains semicolon.', 'client;'],
        ['contains quote', 'client"'],
        ['contains apostrophe', 'client\'test'],
        ['contains less than', 'client<'],
        ['contains greater than', 'client>'],
        ['contains comma', 'client,'],
        ['contains period, dot', 'client.'],
        ['contains question mark', 'client?'],
        ['contains space', 'client test1'],
        ['has mixed alphabet, number, special character', 'client-test!+ test1']
      ], function (description: string, input: string) {
        it(('when the name ' + description), () => {
          component.searchClients(input);
          expect(component.clients.length).toBe(0);
          expect(component.total).toBe(0);
        });
      });

      using([
        ['contains tilde.', '~'],
        ['contains acute, back quote,', '`'],
        ['contains exclamation mark', '!'],
        ['contains ampersat, at', '@'],
        ['contains dollar sign', '$'],
        ['contains percent.', '%'],
        ['contains caret or circumflex.,', '^'],
        ['contains ampersand', '&'],
        ['contains asterisk', '*'],
        ['contains open or left parenthesis.', '('],
        ['contains close or right parenthesis,', ')'],
        ['contains plus', '+'],
        ['contains equal', '='],
        ['contains open brace', '{'],
        ['contains close brace', '}'],
        ['contains open bracket', '['],
        ['contains closed bracket', ']'],
        ['contains pipe', '|'],
        ['contains backslash', '\\'],
        ['contains forward slash', '/'],
        ['contains colon', ':'],
        ['contains semicolon.', ';'],
        ['contains quote', '"'],
        ['contains apostrophe', '\'test'],
        ['contains less than', '<'],
        ['contains greater than', '>'],
        ['contains comma', ','],
        ['contains period, dot', '.'],
        ['contains question mark', '?'],
        ['contains space', '    test1']
      ], function (description: string, input: string) {
        it(('when the name only' + description), () => {
          component.searchClients(input);
          expect(component.clients.length).toBe(0);
          expect(component.total).toBe(0);
        });
      });
    });

    describe('the form should be valid', () => {

      using([
        ['contains numbers range 0-9.', 'client123'],
        ['contains alphabets a-z', 'client-test'],
        ['contains underscore.', 'client_test'],
        ['contains hyphen, minus, or dash.', 'client_test-1'],
        ['has mixed characters', 'client-Test_10']
      ], function (description: string, input: string) {
        it(('when the name only' + description), () => {

          component.searchClients(input);
          expect(component.clients.length).not.toBeNull();
          expect(element.query(By.css('.empty-section'))).toBeNull();
        });
      });
    });
  });

  describe('client list', () => {
    let store: Store<NgrxStateAtom>;
    const availableClients: Client[] = [
      {
        server_id: 'test_server_id',
        org_id: 'test_org_id',
        name: 'viv-win',
        client_name: 'viv-win',
        org_name: 'viveksingh_msys',
        validator: 'false',
        json_class: 'Chef::ApiClient',
        chef_type: 'client',
        client_key: {
          name: 'default',
          public_key: '-----BEGIN PUBLIC KEY-----',
          expiration_date: 'infinity',
          private_key: ''
        },
        certificate: '',
        admin: '',
        uri: ''
      }
    ];
    const emptyClients: Client[] = [];

    beforeEach(() => {
      store = TestBed.inject(Store);
    });

    it('render the client list', () => {
      store.dispatch(
        new GetClientsSuccess({
          clients: availableClients, total: availableClients.length
      }));
      expect(component.clients.length).not.toBeNull();
      expect(element.query(By.css('.empty-section'))).toBeNull();
    });

    it('show no preview image', () => {
      store.dispatch(new GetClientsSuccess({clients: emptyClients, total: emptyClients.length}));
      expect(component.clients.length).toBe(0);
    });
  });
});
