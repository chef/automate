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

<!-- markdownlint-disable-file MD033  -->
<div class="grid-container padding-0">
  <div class="grid-x align-justify small-up-4">
    <div class="cell">
      <div class="card shadow bordered radius margin-1">
        <div class="card-divider">
          Styled Card
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
          Styled Card
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
          Styled Card
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
          Styled Card
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
<div class="reveal" id="exampleModal1" data-reveal>
  <div class="source" contenteditable="true">
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
    <label>HA # ES Nodes:
      <output name="result" for="a b">60</output>
    </label>
    <label>HA Per ES Node Storage (TB):
      <output name="result" for="a b">60</output>
    </label>
    <button class="button copy-button small radius bordered shadow alert margin-top-1">Copy
    </button>
  </div>
  <button class="close-button" data-close aria-label="Close modal" type="button">
    <span aria-hidden="true">&times;</span>
  </button>
</div>