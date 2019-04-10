import BrowserStackTunnel from 'browserstacktunnel-wrapper';

export default class StackTunnel {
  start() {
    return new Promise((resolve, reject) => {
      this.tunnel = new BrowserStackTunnel({
        key: process.env.BROWSERSTACK_KEY,
        hosts: [{
          name: 'localhost',
          port: 9000,
          sslFlag: 0
        }],
        onlyAutomate: true
      });

      console.info('[StackTunnel] Opening tunnel');
      this.tunnel.start((err) => {
        if (err) {
          console.error(`[StackTunnel] Error opening: ${err.message}`);
          reject(err);
        } else {
          console.info('[StackTunnel] Tunnel ready');
          resolve();
        }
      });
    });
  }

  stop() {
    return new Promise((resolve, reject) => {
      console.info('[StackTunnel] Closing tunnel');
      this.tunnel.stop((err) => {
        if (err) {
          console.error(`[StackTunnel] Error closing: ${err.message}`);
          reject(err);
        } else {
          console.info('[StackTunnel] Tunnel closed');
          resolve();
        }
      });
    });
  }
}
