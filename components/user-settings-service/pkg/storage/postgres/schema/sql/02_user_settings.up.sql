UPDATE user_settings
SET settings = '{
          "date_format": {
            "default_value": "ddd, DD MMM YYYY HH:mm:ss [UTC]",
            "value": "ddd, DD MMM YYYY HH:mm:ss [UTC]",
            "enabled": true,
            "valid_values": [
              "ddd, DD MMM YYYY HH:mm:ss [UTC]",
              "YYYY-M-D",
              "ddd, DD MMM YYYY",
              "DD MMM YYYY",
              "ddd, DD MMM",
              "YYYY-MM-DD"
            ]
          }
        }'
WHERE user_name='_default' AND connector='local';