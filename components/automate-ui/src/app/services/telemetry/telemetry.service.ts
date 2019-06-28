import { map, filter } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { HttpClient, HttpErrorResponse, HttpHeaders } from '@angular/common/http';
import { Router, NavigationEnd } from '@angular/router';
import { ReplaySubject, Observable, Subject } from 'rxjs';
import * as Sniffr from 'sniffr';

import { ChefSessionService } from '../chef-session/chef-session.service';
import { ConfigService } from '../config/config.service';
import { CookieService } from 'ngx-cookie';
import { MetadataService } from '../metadata/metadata.service';

declare let analytics: any;

export interface TelemetryData {
  operation: string;
  identifier?: string;
  properties?: Object;
}

@Injectable()
export class TelemetryService {
  // There is a diagram at /dev-docs/diagrams/telemetry-service-ui.png that describes
  // how and where telemetry settings are stored.

  // trackingOperations is a ReplaySubject for us not to miss any tracking calls
  // that are made during the initialization. We want a reasonably small size
  // for the buffer that will be the max number of calls that can be made during
  // initialization.
  private trackingOperations: ReplaySubject<TelemetryData> =
  new ReplaySubject<TelemetryData>(10);

  private telemetryEnabledObservable = new Subject<boolean>();
  public  telemetryEnabled: boolean;
  public  hasTelemetryResponse = false;
  private telemetryUrl;
  private licenseId;
  private anonymousId;
  private product = 'automate';
  private segmentWriteKey;
  private customerName;
  private customerId;
  private licenseType;
  private maxNodes;
  private instanceId;
  private buildVersion;
  private previousUrl: string;
  private currentUrl: string;

  constructor(private httpClient: HttpClient,
    private configService: ConfigService,
    router: Router,
    private cookieService: CookieService,
    private chefSessionService: ChefSessionService,
    private metadataService: MetadataService) {
    // Subscribe to Router's NavigationEnd event to automatically track page
    // browsing of the user.
    router.events.subscribe((event) => {
      if (event instanceof NavigationEnd) {
        this.previousUrl = this.currentUrl;
        this.currentUrl = event.url;
        this.page(this.currentUrl, {previousUrl: this.previousUrl});
      }
    });

    this.configService.getConfig().pipe(
      filter(config => config.telemetryEnabled),
      map((config) => {
        this.telemetryEnabled = config.telemetryEnabled;
        this.telemetryEnabledObservable.next(config.telemetryEnabled);
        this.hasTelemetryResponse = true;
        this.telemetryUrl = config.telemetryUrl;
        this.customerId = config.customerId;
        this.customerName = config.customerName;
        this.licenseId = config.licenseId || configService.defaultLicenseId;
        this.maxNodes = config.maxNodes;
        this.anonymousId = this.cookieService.getObject('ajs_anonymous_id');
        this.instanceId = config.deploymentId || configService.defaultDeployId;
        return this.trackingOperations;
      }))
      .subscribe((trackingOperations) => {
        if (this.chefSessionService.telemetry_enabled) {
          this.engageTelemetry(trackingOperations);
        }
      });
  }

  get enabled(): Observable<boolean> {
    return this.telemetryEnabledObservable;
  }

  setUserTelemetryPreference(isOptedIn: boolean): void {
    if (isOptedIn === true) {
       this.engageTelemetry(this.trackingOperations);
    }
    this.chefSessionService.storeTelemetryPreference(isOptedIn);
  }

  engageTelemetry(trackingOperations: ReplaySubject<TelemetryData>) {
    // We must retrieve the segment write key before we can load analytics.js
        this.retrieveSegmentWriteKey().subscribe(result => {
          if (analytics.initialized) {
            return;
          }
          this.segmentWriteKey = result['write_key'];
          // This loads analytics.js javascript from segment, based on the
          // configured write key. analytics global is initialized by the segment
          // snippet we have in the <head>. analytics.js expects some things from
          // the segment snippet so we load this out of band instead of ng2 style
          // loading. segment snippet buffers the calls made to analytics before
          // it's loaded and properly initialized so this async operation is safe.
          analytics.load(this.segmentWriteKey);

          // We'll treat our Telemetry Pipeline as a custom analytics.js
          // integration and leverage analytics.js emitters to send the
          // data.
          analytics.on('page', (_category, name, properties, _options) => {
            this.emitToPipeline('page', {
              name: name,
              anonymousId: this.anonymousId,
              properties: properties
            });
          });

          analytics.on('track', (event, properties, _options) => {
            this.emitToPipeline('track', {
              userId: this.anonymousId,
              event: event,
              properties: properties
            });
          });

          analytics.on('group', (id, traits, _options) => {
            this.emitToPipeline('group', {
              groupId: id,
              traits: traits,
              anonymousId: this.anonymousId
            });
          });

          analytics.on('identify', (id, traits, _options) => {
            this.emitToPipeline('identify', {
              userId: id,
              traits: traits,
              anonymousId: this.anonymousId
            });
          });

          // First we want to get the build version so we can send it with the
          // metadata to the telemetry pipeline
          this.metadataService
          .getBuildVersion()
          .subscribe(buildVersion => {
            this.buildVersion = buildVersion;

            // For segment the first call we need to make must be identify().
            // In the calls below we might as well call analytics.identify() and
            // analytics.group() but we would like to ensure that we call analytics
            // from a single place so we queue these requests as the first items in
            // our trackingOperations queue.

            // Currently we depend on anonymousId segment creates for us. We should
            // add a userId into this call in the future.
            this.identify();
            // Currently we group users by license ID and customer ID
            this.group(this.licenseId);
            this.group(this.customerId);

            // We want to make sure we have the config and the required calls are
            // queued up before starting to send things into analytics. So we don't
            // subscribe to trackingOperations before these are done.
            trackingOperations.subscribe((trackingData) => {
              analytics[trackingData.operation](trackingData.identifier, trackingData.properties);
            });
            this.trackInitialData();
           });

        },
        ({ status, error: { message } }: HttpErrorResponse) => {
          console.log(`Error retrieving Segment API key: ${status}/${message}`);
        });
  }

  track(event?: string, properties?: any) {
    this.trackingOperations.next({
      operation: 'track',
      identifier: event,
      properties: properties
    });
  }

  page(pageName?: string, properties?: any) {
    this.trackingOperations.next({
      operation: 'page',
      identifier: pageName,
      properties: properties
    });
  }

  // private because we only need to call identify once, and it is done from the
  // constructor.
  private identify(userId?: string, traits?: any) {
    this.trackingOperations.next({
      operation: 'identify',
      identifier: userId,
      properties: traits
    });
  }

  // private because we want to control which groups are created
  private group(groupId?: string, traits?: any) {
    this.trackingOperations.next({
      operation: 'group',
      identifier: groupId,
      properties: traits
    });
  }

  private trackInitialData() {
    this.track('customerName', this.customerName);
    this.track('licenseType', this.licenseType);
    this.track('maxNodes', this.maxNodes);
    this.track('automateVersion', { automateVersion: this.buildVersion});

    const userAgent = new Sniffr();
    userAgent.sniff(navigator.userAgent);
    this.track('userAgentMetadata', {
              browser: userAgent.browser.name,
              browserVersion: userAgent.browser.versionString,
              os: userAgent.os.name,
              osVersion: userAgent.os.versionString
      }
    );
  }

  private emitToPipeline(operation: String, payload: Object) {
    const headers = new HttpHeaders({
      'Content-Type' :  'application/json'
    });

    // JSON SCHEMA:
    // https://github.com/chef/es-telemetry-pipeline/blob/master/schema/event.schema.json
    const json = `{
      "instance_id": "${this.instanceId}",
      "message_version": 1.0,
      "payload_version": 1.0,
      "license_id": "${this.licenseId}",
      "origin": "user-interface",
      "type": "${operation}",
      "product": "${this.product}",
      "product_version": "${this.buildVersion}",
      "install_context": "habitat",
      "timestamp": "${this.getCurrentDateTime()}",
      "payload": ${JSON.stringify(payload)}
    }`;
    this.httpClient.post(this.telemetryUrl + '/events', json, { headers: headers })
      .subscribe(
         _response => {
           // WooHoo! we successfully submitted our telemetry event to the pipeline!
         },
         ({ status, error: { message } }: HttpErrorResponse) => {
           console.log(`Error emitting telemetry event: ${status} - ${message}`);
         }
       );
  }

  private retrieveSegmentWriteKey() {
    return this.httpClient.get(this.telemetryUrl + '/segment/api_keys');
  }

  // ISO 8601 formatted date time
  private getCurrentDateTime() {
    return (new Date).toISOString();
  }
}
