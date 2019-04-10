import sauceConnectLauncher from 'sauce-connect-launcher';

export default class SauceTunnel {
  start() {
    return new Promise((resolve, reject) => {
      console.info('[SauceTunnel] Opening tunnel');
      sauceConnectLauncher({
        username: 'scottopherson',
        accessKey: '6f2d2e5e-d50b-4f42-a713-e1e11ac6a949'
      }, (err, tunnel) => {
        if (err) {
          console.error(`[SauceTunnel] Error opening: ${err.message}`);
          reject(err);
        } else {
          this.tunnel = tunnel;
          console.info('[SauceTunnel] Tunnel ready');
          resolve();
        }
      });
    });
  }

  stop() {
    return new Promise((resolve, reject) => {
      console.info('[SauceTunnel] Closing tunnel');
      this.tunnel.close(() => {
        console.info('[SauceTunnel] Tunnel closed');
        resolve();
      });
    });
  }
}
