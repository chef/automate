import { Stubby } from 'stubby';
import express from 'express';
import proxy from 'proxy-middleware';
import url from 'url';

let mockApiServer;
let mockWebServer;
let mockApiRouter = express.Router();
let mockSseRouter = express.Router();

function startApiServer() {
  return new Promise((resolve, reject) => {
    mockApiServer = new Stubby();

    mockApiServer.start({
      location: 'localhost',
      stubs: 9001,
      admin: 9002,
      tls: 9003
    }, (err) => {
      if (err) {
        reject(err);
      } else {
        mockApiRouter.use(proxy(url.parse('http://localhost:9001')));
        resolve();
      }
    });
  });
}

function clearApiServer() {
  return new Promise((resolve, reject) => {
    mockApiServer.delete(null, (err) => {
      if (err) {
        reject(err);
      } else {
        resolve();
      }
    })
  });
}

function stopApiServer() {
  return new Promise((resolve, reject) => {
    mockApiServer.stop((err) => {
      if (err) {
        reject(err);
      } else {
        resolve();
      }
    });
  });
}

function startWebServer() {
  return new Promise((resolve, reject) => {
    let app = express();
    app.use(express.static('.e2ebuild'));
    app.use(mockSseRouter);
    app.use(mockApiRouter);
    mockWebServer = app.listen(9000, '0.0.0.0', (err) => {
      if (err) {
        reject(err);
      } else {
        resolve();
      }
    });
  });
}

function stopWebServer() {
  return new Promise((resolve, reject) => {
    mockWebServer.close();
    resolve();
  });
}

function mockApi(endpoints) {
  let mockEndpoints = endpoints.map((endpoint) => {
    return new Promise((resolve, reject) => {
      mockApiServer.post(endpoint, (err) => {
        if (err) {
          reject(err);
        } else {
          resolve(endpoint);
        }
      });
    });
  });

  return browser.wait(Promise.all(mockEndpoints));
}

mockApi.clear = function () {
  return browser.wait(clearApiServer());
};

global.mockApi = mockApi;

class MockSseStream {
  start(req, res) {
    this.req = req;
    this.res = res;
    this.res.writeHead(200, {
      'Content-Type': 'text/event-stream',
      'Cache-Control': 'no-cache',
      'Connection': 'keep-alive'
    });
    this.res.write('\n');
  }

  send(name, data, id) {
    this.res.write(`event: ${name}\n`);
    if (id) { this.res.write(`id: ${id}\n`); }
    this.res.write(`data: ${JSON.stringify(data)}\n\n`);
  }

  disconnect() {
    this.res.end();
  }

  reconnect() {
    this.res.write('\n');
  }
}

function mockSse(endpoint) {
  let mockStream = new MockSseStream();
  mockSseRouter.get(endpoint, (req, res) => {
    mockStream.start(req, res);
  });
  return mockStream;
}

mockSse.clear = function () {
  mockSseRouter.stack = [];
}

global.mockSse = mockSse;

export default class MockServer {

  start() {
    return Promise.all([
      startApiServer(),
      startWebServer()
    ]);
  }

  stop() {
    return Promise.all([
      stopApiServer(),
      stopWebServer()
    ]);
  }
}
