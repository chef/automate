+++
title = "Storage Calculator Prototype"
draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Storage Calculator"
    parent = "automate/getting_started"
    identifier = "automate/getting_started/calculator.md Calculator"
    weight = 15
+++

Use this calculator to determine the storage capacity you need to support your Chef Automate installation.

TODO:

1. Describe the calculator in terms of context + action + impact
2. What does this information mean? Are the classifications distinct (Do the nodes with chef run data also have compliance data, etc?)
2. What is the takeaway (the action needed based on estimated # of ES notes)

- Is there one number that the user needs? Where should the user go and act on the information
- Whatever that information is, it needs to be REALLY CLEAR on the copy page
- tool tips that explain each category
- Explain what node Data Size is and where to find it
- Explain Run interval and where to find it?
- How does run interval interact with event feed data
- Where does Service Groups data come from and how does a user find it?
- Do the spreadsheet calculations reflect reality?

Chef Infra Client Run
node count: automate/infrastructure/client-runs
Node data size: ??

Compliance
node count: automate/compliance/reports/overview
Node data size: ??

Event Feed
node count: automate/dashboards/event-feed
Node data size: ??

Service Groups
node count: automate/applications/service-groups...but this gives me a count of groups, not of nodes
Node data size: ??

<!-- markdownlint-disable-file MD033  -->
<div class="grid-container padding-0">
  <div class="grid-x align-justify large-up-4 small-up-1">
    <div class="cell">
      <div class="card shadow bordered radius margin-1">
        <div class="card-divider">
          Chef Infra Client Run Data
        </div>
        <div class="card-section">
            <label>Number of Nodes
              <input type="number" placeholder="1000">
            </label>
            <label>Node Data Size (MB)
              <input type="number" placeholder="0.25">
            </label>
            <label>Run Interval (HR)
              <input type="number" placeholder="1">
            </label>
            <label> Data Retention (Days)
              <input type="number" placeholder="30">
            </label>
        </div>
        <div class="card-divider margin-0">
          <p class="font-bold padding-right-1">HA</p>
          <div class="switch small rounded">
            <input class="switch-input" id="exampleSwitch" type="checkbox" name="exampleSwitch">
            <label class="switch-paddle" for="exampleSwitch">
            </label>
          </div>
        </div>
        <div class="card-section">
          <label> # of ES Nodes
          <input type="number" placeholder="3">
          </label>
        </div>
      </div>
    </div>
    <div class="cell">
      <div class="card shadow bordered radius margin-1">
        <div class="card-divider">
          Compliance Run Data
        </div>
        <div class="card-section">
            <label>Number of Nodes
              <input type="number" placeholder="1000">
            </label>
            <label>Node Data Size (MB)
              <input type="number" placeholder="0.25">
            </label>
            <label>Run Interval (HR)
              <input type="number" placeholder="1">
            </label>
            <label> Data Retention (Days)
              <input type="number" placeholder="30">
            </label>
        </div>
        <div class="card-divider margin-0">
          <p class="font-bold padding-right-1">HA</p>
          <div class="switch small rounded">
            <input class="switch-input" id="exampleSwitch" type="checkbox" name="exampleSwitch">
            <label class="switch-paddle" for="exampleSwitch">
            </label>
          </div>
        </div>
        <div class="card-section">
          <label> # of ES Nodes
          <input type="number" placeholder="3">
          </label>
        </div>
      </div>
    </div>
    <div class="cell">
      <div class="card shadow bordered radius margin-1">
        <div class="card-divider">
          Event Feed Data
        </div>
        <div class="card-section">
            <label>Number of Nodes
              <input type="number" placeholder="1000">
            </label>
            <label>Node Data Size (MB)
              <input type="number" placeholder="0.25">
            </label>
            <label>Run Interval (HR)
              <input type="number" placeholder="1">
            </label>
            <label> Data Retention (Days)
              <input type="number" placeholder="30">
            </label>
        </div>
        <div class="card-divider margin-0">
          <p class="font-bold padding-right-1">HA</p>
          <div class="switch small rounded">
            <input class="switch-input" id="exampleSwitch" type="checkbox" name="exampleSwitch">
            <label class="switch-paddle" for="exampleSwitch">
            </label>
          </div>
        </div>
        <div class="card-section">
          <label> # of ES Nodes
          <input type="number" placeholder="3">
          </label>
        </div>
      </div>
    </div>
    <div class="cell">
      <div class="card shadow bordered radius margin-1">
        <div class="card-divider">
          Service Groups Data
        </div>
        <div class="card-section">
            <label>Number of Nodes
              <input type="number" placeholder="1000">
            </label>
            <label>Node Data Size (MB)
              <input type="number" placeholder="0.25">
            </label>
            <label>Run Interval (HR)
              <input type="number" placeholder="1">
            </label>
            <label> Data Retention (Days)
              <input type="number" placeholder="30">
            </label>
        </div>
        <div class="card-divider margin-0">
          <p class="font-bold padding-right-1">HA</p>
          <div class="switch small rounded">
            <input class="switch-input" id="exampleSwitch" type="checkbox" name="exampleSwitch">
            <label class="switch-paddle" for="exampleSwitch">
            </label>
          </div>
        </div>
        <div class="card-section">
          <label> # of ES Nodes
          <input type="number" placeholder="3">
          </label>
        </div>
      </div>
    </div>
  </div>
</div>
<div class="grid-container">
  <div class="grid-x align-right">
    <button type="button" class="button large radius bordered shadow primary" data-open="exampleModal1">Calculate</button>
  </div>
</div>
<div class="reveal padding-3" id="exampleModal1" data-reveal>
  <div class="warning callout small">Copy before closing results</div>
  <div class="source" contenteditable="true">
    <div class="grid-x align-right">
      <button class="button copy-button width-100 grid-x align-right medium radius bordered shadow primary margin-bottom-0">Copy
      </button>
    </div>
    <h4>Chef Infra Client Run Data</h4>
      <label>Number of Nodes:
        <output name="result" for="a b">60</output>
      </label>
      <label>Node Data Size (MB):
        <output name="result" for="a b">60</output>
      </label>
      <label>Run Interval (HR):
        <output name="result" for="a b">60</output>
      </label>
      <label>Data Retention (Days):
        <output name="result" for="a b">60</output>
      </label>
      <label>Total Number of Nodes:
        <output name="result" for="a b">60</output>
      </label>
      <label>Total Node Data:
        <output name="result" for="a b">60</output>
      </label>
      <label>Storage/Day (TB):
        <output name="result" for="a b">60</output>
      </label>
    <h4>Compliance Run Data</h4>
      <label>Number of Nodes:
        <output name="result" for="a b">60</output>
      </label>
      <label>Node Data Size (MB):
        <output name="result" for="a b">60</output>
      </label>
      <label>Run Interval (HR):
        <output name="result" for="a b">60</output>
      </label>
      <label>Data Retention (Days):
        <output name="result" for="a b">60</output>
      </label>
      <label>Total Number of Nodes:
        <output name="result" for="a b">60</output>
      </label>
      <label>Total Node Data:
        <output name="result" for="a b">60</output>
      </label>
      <label>Storage/Day (TB):
        <output name="result" for="a b">60</output>
      </label>
    <h4>Event Feed Data</h4>
      <label>Number of Nodes:
        <output name="result" for="a b">60</output>
      </label>
      <label>Node Data Size (MB):
        <output name="result" for="a b">60</output>
      </label>
      <label>Run Interval (HR):
        <output name="result" for="a b">60</output>
      </label>
      <label>Data Retention (Days):
        <output name="result" for="a b">60</output>
      </label>
      <label>Total Number of Nodes:
        <output name="result" for="a b">60</output>
      </label>
      <label>Total Node Data:
        <output name="result" for="a b">60</output>
      </label>
      <label>Storage/Day (TB):
        <output name="result" for="a b">60</output>
      </label>
    <h4>Service Group Data</h4>
      <label>Number of Nodes:
        <output name="result" for="a b">60</output>
      </label>
      <label>Node Data Size (MB):
        <output name="result" for="a b">60</output>
      </label>
      <label>Run Interval (HR):
        <output name="result" for="a b">60</output>
      </label>
      <label>Data Retention (Days):
        <output name="result" for="a b">60</output>
      </label>
      <label>Total Number of Nodes:
        <output name="result" for="a b">60</output>
      </label>
      <label>Total Node Data:
        <output name="result" for="a b">60</output>
      </label>
      <label>Storage/Day (TB):
        <output name="result" for="a b">60</output>
      </label>
    <label>HA Total Storage (TB):
      <output name="result" for="a b">60</output>
    </label>
  <h4>High Availability</h4>
    <label>HA # ES Nodes:
      <output name="result" for="a b">60</output>
    </label>
    <label>HA Per ES Node Storage (TB):
      <output name="result" for="a b">60</output>
    </label>
  </div>
  <button class="close-button" data-close aria-label="Close modal" type="button">
    <span aria-hidden="true">&times;</span>
  </button>
</div>
