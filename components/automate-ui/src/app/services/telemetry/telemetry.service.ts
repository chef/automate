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
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Store } from '@ngrx/store';

import { UpdateUserPreferencesSuccess, UpdateUserPreferencesFailure } from 'app/services/user-preferences/user-preferences.actions';
import { ClientRunsStatsService } from './client-runs-stats/client-runs-stats.service';
import { ComplianceStatsService } from './compliance-stats/compliance-stats.service';
import { NodeUsageStats, NodeUsageAckStats } from './compliance-stats/compliance-stats.model';
import { ApplicationUsageStats, ApplicationUsageAckStats } from './application-stats/application-stats.model';
import { ApplicationStatsService } from './application-stats/application-stats.service';

declare let analytics: any;

export interface TelemetryData {
  operation: string;
  identifier?: string;
  properties?: Object;
}

declare let pendo: any;

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
  private  telemetryCheckboxObservable = new Subject<boolean>();
  private isSkipNotification = false;
  private deploymentType: string;

  constructor(private httpClient: HttpClient,
    private configService: ConfigService,
    router: Router,
    private cookieService: CookieService,
    private chefSessionService: ChefSessionService,
    private metadataService: MetadataService,
    private store: Store<NgrxStateAtom>,
    private complianceStatsService: ComplianceStatsService,
    private applicationStatsService: ApplicationStatsService,
    private clientRunsStatsService: ClientRunsStatsService) {
    // Subscribe to Router's NavigationEnd event to automatically track page
    // browsing of the user.
    router.events.subscribe((event) => {
      if (event instanceof NavigationEnd) {
        this.previousUrl = this.currentUrl;
        this.currentUrl = event.url;
        this.page(this.currentUrl, {previousUrl: this.previousUrl});
      }
    });

    if(pendo) {
      pendo.initialize({
        visitor: {
          id: 'Automate anonymouse user'
        }
      })
    }

    this.configService.getConfig().pipe(
      filter(config => { 
        return config.telemetryEnabled
      }),
      map((config) => {
        this.telemetryEnabled = config.telemetryEnabled;
        this.telemetryEnabledObservable.next(config.telemetryEnabled);
        this.hasTelemetryResponse = true;
        this.telemetryUrl = config.telemetryUrl;
        this.customerId = config.customerId;
        this.customerName = config.customerName;
        this.licenseId = config.licenseId || configService.defaultLicenseId;
        this.maxNodes = config.maxNodes;
        
        this.instanceId = config.deploymentId || configService.defaultDeployId;
        this.deploymentType = config.deploymentType;
        try{
          this.anonymousId = this.cookieService.getObject('ajs_anonymous_id');
        } catch(e) {
          console.log('Unable to fetch ajs_anonymous_id from config ', e)
        }
        return this.trackingOperations;
      }))
      .subscribe((trackingOperations) => {
        this.initialisePendo();
        this.initiateTelemetry(trackingOperations);
      });
  }

  async initialisePendo() {
    console.log('initialisePendo');
    try {
      if(pendo) {
        pendo.identify({
          visitor: {
            id: this.customerId,
            full_name: this.customerName
        },
        account: {
            id: this.licenseId,
            name: this.customerName,
            maxNodes: this.maxNodes,
            instanceId: this.instanceId,
            deploymentType: this.deploymentType,
            anonymousId: this.anonymousId,
            automateVersion: this.buildVersion
        }
      });
      }
    } catch(e) {
      console.log('Unable to initialise Pendo ', e);
    }
    
  }

  trackPendo(event?: string, properties?: any) {
    try {
      if(pendo) {
        pendo.track(event, properties);
      }
    }catch(e) {
      console.log('Unable to send track events to Pendo ', e);
    }
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
        if (analytics.initialized) {
          return;
        }
        // We must retrieve the segment write key before we can load analytics.js
        this.retrieveSegmentWriteKey().subscribe(result => {
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
            if (properties) {
              if (properties.referrer) {
                properties.referrer = this.sanitizeDomainURL(properties.referrer);
              }
              if (properties.url) {
                properties.url = this.sanitizeDomainURL(properties.url);
              }
            }
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

          analytics.sanitizeDomainURL = this.sanitizeDomainURL;
          analytics.addSourceMiddleware(this.middleware);
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
          if (!this.isSkipNotification) {
            this.store.dispatch(new UpdateUserPreferencesSuccess('Updated user preferences.'));
          }

        },
        ({ status, error: { message } }: HttpErrorResponse) => {
          console.log(`Error retrieving Segment API key: ${status}/${message}`);
          if (!this.isSkipNotification) {
            this.store.dispatch(new UpdateUserPreferencesFailure(message));
          }
        });
  }

  track(event?: string, properties?: any) {
    if(pendo) {
      this.trackPendo(event, properties);
    }
    this.trackingOperations.next({
      operation: 'track',
      identifier: event,
      properties: properties
    });
  }

  page(pageName?: string, properties?: any) {
    properties.url = '';
    properties.referrer = '';
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

  private emitToPipeline(operation: String, payload: Object, isReturnHttp?) {
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
      "deployment_type": "${this.deploymentType}",
      "timestamp": "${this.getCurrentDateTime()}",
      "payload": ${JSON.stringify(payload)}
    }`;

    if (isReturnHttp) {
      return this.httpClient
        .post(this.telemetryUrl + '/events', json, { headers, params: { unfiltered: 'true' } });
    }

    this.httpClient.post(this.telemetryUrl + '/events', json, { headers, params: { unfiltered: 'true' } })
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
    return this.httpClient.get(this.telemetryUrl + '/segment/api_keys',
     { params: { unfiltered: 'true' }}); // don't pass 'projects' header
  }

  // ISO 8601 formatted date time
  private getCurrentDateTime() {
    return (new Date).toISOString();
  }

  sanitizeDomainURL(url) {
    let restByDot: any;
    let firstByDot: string;
    [firstByDot, ...restByDot] = url.split('.');
    restByDot = restByDot.join('.');
    if (restByDot.indexOf('/') > -1) {
      let [firstBySlash, ...restBySlash] = restByDot.split('/');
      restBySlash = restBySlash.join('/');
      firstBySlash = '***';
      return firstByDot + '.' + firstBySlash + '/' + restBySlash;
    } else {
      restByDot = '***';
      return firstByDot + '.' + restByDot;
    }
  }

  middleware({ payload, next }) {
    if (payload && payload.obj && payload.obj.context && payload.obj.context.page) {
      const page = payload.obj.context.page;
      if (page.referrer) {
        page.referrer = analytics.sanitizeDomainURL(page.referrer);
      }
      if (page.url) {
        page.url = analytics.sanitizeDomainURL(page.url);
      }
    }
    next(payload);
  }

  getTelemetryCheckboxObservable() {
    return this.telemetryCheckboxObservable;
  }

  updateTelemetryCheckbox(telemetryPref: boolean) {
    this.telemetryCheckboxObservable.next(telemetryPref);
  }

  async initiateTelemetry(trackingOperations) {
    try {
      await this.fetchBuildVersion();
    } catch (error) {
      console.log(error);
    }
    try {
      await this.handleTelemetryNodeStats();
    } catch (error) {
      console.log(error);
    }
    if (this.chefSessionService.telemetry_enabled) {
      this.isSkipNotification = true;
      this.engageTelemetry(trackingOperations);
    }
  }

  fetchBuildVersion() {
    let resolver;
    const promise = new Promise((resolve) => {
      resolver = resolve;
    });
    const metadataSubscription = this.metadataService.getBuildVersion()
      .subscribe((buildVersion) => {
        this.buildVersion = buildVersion;
        if (metadataSubscription) {
          metadataSubscription.unsubscribe();
        }
        resolver('success');
    });
    return promise;
  }

  async handleTelemetryNodeStats() {
    let resolver;
    const promise = new Promise((resolve) => {
      resolver = resolve;
    });
    // compliance stats
    try {
      const nodeUsageStats: NodeUsageStats = await this.complianceStatsService
        .getComplianceStats();
      if (nodeUsageStats && Number(nodeUsageStats['days_since_last_post']) > 0) {
        const ackStats: NodeUsageAckStats = await this
        .sendNodeStatsToTelemetry(nodeUsageStats, 'complianceTargetCountsGlobal');
        await this.complianceStatsService.sendAcknowledgement(ackStats);
      }
    } catch (error) {
      console.log(error);
    }
    // client runs stats
    try {
      const nodeUsageStats: NodeUsageStats = await this.clientRunsStatsService
        .getClientRunsStats();
      if (nodeUsageStats && Number(nodeUsageStats['days_since_last_post']) > 0) {
        const ackStats: NodeUsageAckStats = await this
        .sendNodeStatsToTelemetry(nodeUsageStats, 'clientRunPureCountGlobal');
        await this.clientRunsStatsService.sendAcknowledgement(ackStats);
      }
    } catch (error) {
      console.log(error);
    }
    // application stats
    try {
      const applicationUsageStats: ApplicationUsageStats = await this.applicationStatsService
        .getApplicationStats();
        if (applicationUsageStats && Number(applicationUsageStats['days_since_last_post']) > 0) {
          const ApplicationAckStats: ApplicationUsageAckStats = await this
          .sendApplicationStatsToTelemetry(applicationUsageStats);
          await this.applicationStatsService.sendAcknowledgement(ApplicationAckStats);
        }
    } catch (error) {
      console.log(error);
    }
    resolver('success');
    return promise;
  }

  sendNodeStatsToTelemetry(nodeUsageStats: NodeUsageStats, eventName: string)
  : Promise<NodeUsageAckStats> {
    let resolver;
    const promise = new Promise<NodeUsageAckStats>((resolve) => {
      resolver = resolve;
    });
    const nodeUsageStatsSubscription = this.emitToPipeline('track', {
      userId: this.anonymousId,
      event: eventName,
      properties: { node_cnt: nodeUsageStats.node_cnt, deployment_type: this.deploymentType }
    }, true).subscribe(() => {
      if (nodeUsageStatsSubscription) {
        nodeUsageStatsSubscription.unsubscribe();
      }
      resolver({lastTelemetryReportedAt: this.getCurrentDateTime()});
    },
    ({ status, error: { message } }: HttpErrorResponse) => {
      console.log(`Error emitting telemetry event: ${status} - ${message}`);
      resolver('error');
    });
    return promise;
  }

  sendApplicationStatsToTelemetry(applicationUsageStats: ApplicationUsageStats)
  : Promise<NodeUsageAckStats> {
    let resolver;
    const promise = new Promise<ApplicationUsageAckStats>((resolve) => {
      resolver = resolve;
    });
    const applicationUsageStatsSubscription = this.emitToPipeline('track', {
      userId: this.anonymousId,
      event: 'servicesCountsGlobal',
      properties: {
        total_services: applicationUsageStats.total_services,
        deployment_type: this.deploymentType
      }
    }, true).subscribe(() => {
      if (applicationUsageStatsSubscription) {
        applicationUsageStatsSubscription.unsubscribe();
      }
      resolver({lastTelemetryReportedAt: this.getCurrentDateTime()});
    },
    ({ status, error: { message } }: HttpErrorResponse) => {
      console.log(`Error emitting telemetry event: ${status} - ${message}`);
      resolver('error');
    });
    return promise;
  }

}
