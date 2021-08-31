# Changes to this file will be overwritten
# Add any overrides via:
#  systemctl edit hab-sup
#  systemctl daemon-reload

[Unit]
Description=Habitat-Supervisor
After=network-online.target local-fs.target
Wants=network-online.target local-fs.target

[Service]
Type=simple
ExecStartPre=-/bin/rm -f /hab/sup/default/LOCK
ExecStart=/bin/bash -c ". /hab/sup/default/SystemdEnvironmentFile.sh; exec /bin/hab sup run ${hab_sup_run_args}"
Restart=on-failure
RestartSec=10
LimitNOFILE=262144
KillMode=cgroup
ExecStop=/bin/hab sup term
UMask=0022

[Install]
WantedBy=multi-user.target
