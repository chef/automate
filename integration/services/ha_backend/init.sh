#!/bin/bash

HA_BACKEND_DIR=$(dirname "${BASH_SOURCE[0]}")
HA_BACKEND_USER="admin"
# changing to match the default in automate-ha-backend
HA_BACKEND_PASSWORD="admin"

ha_backend_container1=$(service_container_name "ha_backend_1")
ha_backend_container2=$(service_container_name "ha_backend_2")

ha_backend_private=$(service_config_path "ha_backend_private")
ha_backend_config=$(service_config_path "ha_backend.toml")
ha_admin_pg_password='thisisapassword%u'

ha_backend_setup() {
    mkdir -p "$ha_backend_private"
    mkdir -p "$(dirname "$ha_backend_config")"
    mkdir -p $(service_config_path "ha_backend_backups")
    chmod -R 0777 $(service_config_path "ha_backend_backups")
    chmod -R 0777 $(service_config_path "")
    local peer_ring_file
    peer_ring_file=$(service_config_path "ha_backend_peers")
    touch "$peer_ring_file"

    local ha_backend_container1_ip ha_backend_container2_ip
    docker_run "$ha_backend_container1" "chefes/centos-systemd:latest"
    ha_backend_container1_ip=$(container_ip "$ha_backend_container1")
    log_info "Launched $ha_backend_container1 with ip ${ha_backend_container1_ip}"
    echo "$ha_backend_container1_ip" >> "$peer_ring_file"

    docker_run "$ha_backend_container2" "chefes/centos-systemd:latest"
    ha_backend_container2_ip=$(container_ip "$ha_backend_container2")
    log_info "Launched $ha_backend_container2 with ip ${ha_backend_container2_ip}"

    echo "Generating elasticsearch ssl keys"

    # copy-pasta'd from a2-ha-backend
    certdir="$HA_BACKEND_DIR/certificates"
    mkdir -p "$certdir"
    echo "$certdir"

    #shellcheck disable=SC2034
    RANDFILE="$certdir"/.rnd
    
    echo -e "-----BEGIN CERTIFICATE-----\nMIIDqzCCApOgAwIBAgIUMUG0kbGT5s8Vo415aNfii81CzRcwDQYJKoZIhvcNAQEL\nBQAwZTELMAkGA1UEBhMCVVMxEzARBgNVBAgMCldhc2hpbmd0b24xEDAOBgNVBAcM\nB1NlYXR0bGUxGjAYBgNVBAoMEUNoZWYgU29mdHdhcmUgSW5jMRMwEQYDVQQDDApj\naGVmcm9vdGNhMB4XDTIyMDYwMTA3MzMwMFoXDTI1MDUzMTA3MzMwMFowZTELMAkG\nA1UEBhMCVVMxEzARBgNVBAgMCldhc2hpbmd0b24xEDAOBgNVBAcMB1NlYXR0bGUx\nGjAYBgNVBAoMEUNoZWYgU29mdHdhcmUgSW5jMRMwEQYDVQQDDApjaGVmcm9vdGNh\nMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA4aKz6j+Otgeg+oZSsHtq\nz5Phyb2cWr+CdbLb2qZA9ZDcCjEzkxvuvI6QBScF9feVk7YVXg59DXlHJRP7AHx5\nnG5iQmND0jUUQGjyZfKTWO8Z9F8D/w0HAFOk4LTvVR/AAC6f9PpxjaydfSzkh5cc\nNxeotjlkYUgh6D7fFma9gRtjX4a36miA5FqCv0Lkwrk9tXHdDk6skdjBvCS0URnq\nzDfyTcK7R/L/iaGYtY43c8tB8eHKwK+ZQ2fR4V90YI3xkbkwr6j0efZyU2Kp/03r\n2nDtFQXHwwuj6Sg465DJB9MQ01IbR30NUjJiFGOJaxdRlhsEeWWNL8BELQuGpJe9\nVQIDAQABo1MwUTAdBgNVHQ4EFgQU3XAfDSRK1wCTf0wiDjlR4m06FCQwHwYDVR0j\nBBgwFoAU3XAfDSRK1wCTf0wiDjlR4m06FCQwDwYDVR0TAQH/BAUwAwEB/zANBgkq\nhkiG9w0BAQsFAAOCAQEAVTiFfpOzYfXzQxTrl4VxctdQJI52jPjWP55PmK/IO7sb\nn/DKzRNnvn7mN3EDGmshh5i76/hoGA09pK4SdAwJl2QIRJSu3ChH4QCf7n/iIYww\nSxhpOp9QtJA5Cyu4MoemK49Lld/7xf3Qdt1pOgEMz4AGLt2uwS5SdmR4OkSPHqt0\nQq/lgSoiawVOd0UE+5Ocu5S472du/REcVS4KkQdkzZaw9Q7OGIN9G0X0wb0V3UZs\nR2XuApUUaGOl0s0a1uNMv3WkyOrCGS3JkIem6+R59pfliaz6FPcQD+0oXI2gjG3c\nH4VFwOis/oT/2FwYM89j7XZPDpRfUUGMHD53W7YPHA==\n-----END CERTIFICATE-----" > "$certdir/MyRootCA.pem"
    echo -e "-----BEGIN CERTIFICATE-----\nMIIDeDCCAmCgAwIBAgIUDsi0qPmptSvcCCXXJ2Fm1pFmvz8wDQYJKoZIhvcNAQEL\nBQAwZTELMAkGA1UEBhMCVVMxEzARBgNVBAgMCldhc2hpbmd0b24xEDAOBgNVBAcM\nB1NlYXR0bGUxGjAYBgNVBAoMEUNoZWYgU29mdHdhcmUgSW5jMRMwEQYDVQQDDApj\naGVmcm9vdGNhMB4XDTIyMDYwMTA3MzMwMFoXDTI1MDUzMTA3MzMwMFowZDELMAkG\nA1UEBhMCVVMxEzARBgNVBAgMCldhc2hpbmd0b24xEDAOBgNVBAcMB1NlYXR0bGUx\nGjAYBgNVBAoMEUNoZWYgU29mdHdhcmUgSW5jMRIwEAYDVQQDDAljaGVmYWRtaW4w\nggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQCmAPx686ekg1SaD1FRmkLW\nuIt4i7aoOFm/pAd7teD6AH4Xa3pa3w8ePNljz92xc4FciXakmNoBhiWl1gGff6xk\nzyEJkrj1+EYChIMo82Uu+/WBPVLevhNMQkWYL/tG2UGJNYKUtvKeUoNS9KFLc+6r\n0riD/fnP08+3SlK6o+ZzQAbqwQPAGjfLeGjduNPDJgvulbt98sPS5HPyme2PUk+z\nTg2YEoCI5U/op+OcC7S0HyjiG8IkfU300gpH70iwqDLdZ5KSnonKaxxhtEs5MR3W\nFP3wwh51Dbs43qF39nZMQfUCdOn6VCdH6ZlMv8CKvbpXm7w0TkARUky4FJlnUl2X\nAgMBAAGjITAfMB0GA1UdJQQWMBQGCCsGAQUFBwMCBggrBgEFBQcDATANBgkqhkiG\n9w0BAQsFAAOCAQEAQ1GYvQvZHSLd0dVQq91T+yBP15HUuhd/sU4HXCDWNrlpp28f\nAWNwKW5L+p9Xt7DLVOFIl9WR1QowNa6h1E3sDZyAzM2m0wBq9qGzBosJzG+wQkhM\n8v0Rx6ODVw5yy3LxUEEyMQwYH++/aFz3dB+EbH74XFgPYGq4VcpZA5dkFYdzAELA\nT/34HKUpMV6KCI+DQRFKSBOZQnN1WsJs9zcxkl3FIGL7H4IifbmAwzSGS+zDRiHt\nQpc8KJQvH5IQPHam452H5RguC4EaFBJjOe1gg487/dj+kZRRGw+MnFo4Lpp+Ce32\nyx68zYBUuwdHlQEYkDjTQVceKl68CpOKhE2tQw==\n-----END CERTIFICATE-----" > "$certdir/odfe-admin.pem"
    echo -e "-----BEGIN PRIVATE KEY-----\nMIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQCmAPx686ekg1Sa\nD1FRmkLWuIt4i7aoOFm/pAd7teD6AH4Xa3pa3w8ePNljz92xc4FciXakmNoBhiWl\n1gGff6xkzyEJkrj1+EYChIMo82Uu+/WBPVLevhNMQkWYL/tG2UGJNYKUtvKeUoNS\n9KFLc+6r0riD/fnP08+3SlK6o+ZzQAbqwQPAGjfLeGjduNPDJgvulbt98sPS5HPy\nme2PUk+zTg2YEoCI5U/op+OcC7S0HyjiG8IkfU300gpH70iwqDLdZ5KSnonKaxxh\ntEs5MR3WFP3wwh51Dbs43qF39nZMQfUCdOn6VCdH6ZlMv8CKvbpXm7w0TkARUky4\nFJlnUl2XAgMBAAECggEATgAkonkeinSTKhiMczVytoXcMvYjC4P+18dxJLXfUlks\n5kBAK7an2eNpmjavypOlcx+8ObcAqLqpsfSOqq1JCc0xjtf8RMc1zjUJlBJZtuv4\n/ZMc/0B8vNZe8RWKkk2/N4OHKe2UrsPCpRYBO/zrZp0KyipqWTcDYPJxUPV2cSwg\nu8S+O6duDMteON4Ngo4mhsF2OsS2IRKe5dUptNZE9Zr/c7lJiapRkCu77YOSIud1\nQ9IhPOuhTOmZ5oGiCPiupFhHfbxIc9dTtQFy+q0ngBr3Wao3wH3A6yrSJkfNceF6\nEepFwo9S3ENr6e+dKyYEYRqLn5y5xp0tWxnd6ftAoQKBgQDVYDoa+WPzNc3eW21I\nuOnLWsGSYpYOMjR9yfQPAWcehEXQW138UV2KXJpPMyr1SIj1Uce3D5fcSfoi93yO\n7Bo9t1NI6bcrwCdTyTYwDE4uob3HJcDjYrndLkKq14fuAuiqzIfcbin7h3YyLtHn\nQKb5EM2nbdl/ZSHqt9uv4eXTuQKBgQDHKjaejTyQRGIsbwGErv3G7fp1yXOdDirZ\nX9TG4Mk1gfdSNsNnR1ftOxIVsBgIvVwbS3/Hc9O33ERNZplC+3Mb3RSr6AUxpWBV\n0cL9h28UI2kxInskVk0jcy0aUJuXwHzJwissMzhO7+abbfk6lP+hnDVPTLJK9i+m\nyGnJN4kDzwKBgELrfQ28rZY3h6t3LR0jsV9vxUnt1XX6VPAmiH60R+0QOv0Pyrg5\npIaiaFryh0SKTTnzPhmxsEUMhoC08cG5VrmIZ3x0oFf+WdczQeVr1pFVdLmio44t\nBnHR8mK2WhFRpDr9DpcZjd0vLOisOnS2mCeBF56tGwuTYkHFDAdrZ7MxAoGAdwGK\nUYMjRTEXWGWcCqRlj5BzsB4Hrzjf8bOJeKyojghyt/VxHS1MJLy/OaZXNiGv/79F\nQvC6+YnsZjOAML3/Mg8QYwfo0pl2ztDgKo3ambXvDSUFX9kRjrUrpiW0UyD5b7VO\nBsM0SjafHRDBc3fEp9SnJhM2FgS0xHvMeids/5ECgYEAoMz3QW4sslG3cgXEDhFw\nVu6UUMNuGW2b9ZedgSafkp6pXVVwMMf1eRN3Wz50MIDmMPnlYy0qjf5ooP3kG8F2\nKrhkYVqH3Iuchwz/JpT+fSfoRsheUVPSLJ44Kd5PldlUD5YEPlyWMrH+I49wLXdG\nfiRKmwfrHJg+yKmkdvA/ezY=\n-----END PRIVATE KEY-----" > "$certdir/odfe-admin.key"
    echo -e "-----BEGIN PRIVATE KEY-----\nMIIEvgIBADANBgkqhkiG9w0BAQEFAASCBKgwggSkAgEAAoIBAQDOYigPPMtjO0/j\nkg9rz0zxSZkqChxMmd1PKltWS/TkWPXOci6R2s1iEOPEtQHfznoIK62RuyVjowWN\nWmLl1oD3vGUPZ0t5lHRLt0I1udsyW+U6fKjuzCRlVxTWgRqCg0ryLhqZ5a7jWC4s\nCW049Js5AkSr0lwoO4jrMx7e9lVJH0UDOfYl4dy4zbqekz1TJlvCGps3u/zQfQzM\n+CMY5JJ33Dvb9RadhPhiaJ6WyDwWRiLQXpTDh9IPxCATAEpdxl+PngS53YXHY7Hb\ncxFf3yROh8IDLSRmoW/te87SyueHZGSkOxEu1lZSKtwX8PsnqamiOrZ3yKa6kmj3\nJZnlvmrFAgMBAAECggEBAJMQRtN00smHPko/jlWYho95jnbydW/wRE9hM2yu+3as\nHZxaqOyuKaQzvNoy0X8VtseTuD8cSGxTgmVTfx2gWI4v5SOwy4t7SdOaZse6BIRC\nD3o50nc3aMf94Y9ot+dAf2tLhxEbJiBCecRPg1EOnH1nLF/zqk/Q/KP87YabqfJL\n2hV/+ZbUtRzyC88X7zgfHOIPjKF6qZYj2rBF9d6J/nyk4+RGTQXuYMRXRfMa/+Vg\nPQA8vHLOF1ZGbDfLkATcRFy8bVaCHbESRJuVxx377mK3fsZJwP3en3dsANkurJFU\npf0hdW2rL+XoKyBgkd5ZC1XMW6nLBJkhnyuDkWSLwIECgYEA77vdAyXOBIrykR0P\nTnG7d79h3iccYtMEJt3+yK+i/z4xvJ4kUyujXfld+DC2EpudGvAfcPqswMcEfASU\nz7/S7Fjs1DoNqhzVQ2AbLtiXv6gDDqtxRhiQsRebDrCMsy3ak9S+qwx1po/GbAI4\n+Jyd1Ej7nFbr1jaRDE5v/NCkpWkCgYEA3GMAglhMyiiZIp1SCTyZlw/JsgeounW8\nKpCeJW6sRGeKhjU63K6JAKRVH8ZN4BCZAslHgyBlqFavRP1iNGz8aVzijuE3LYv9\ndjClbTKDeL5BVQCMZmMeHRx26A6C2VR3bCeogKCLG2p2w2e6aNyC/5vRswPQ9fHN\n9oQTdP+3Iv0CgYBhsaoARWCuQz1UW4ZITlq6Eti8Ii4UK6/uctsIR2LiJyM3nS5q\nYkch3aQopxkbfiTIvH/jl5Pcv32ZWHiYLLC0tgxU9A9renKCEWEidwtoA/yy9rSj\nfxE/ycHbe6Dqx83iHnlQ1TQAm2idzU4QKiq7Pr6iWF2lHvl/DVvfp9rguQKBgHeX\nNZCFr+3HiAmziu++Na42gAGejF3aQ6hpX5f6bXqVISNgwTc9ctSpWBPs/RdIim9r\nn+W079UgbO2n/eFq/rGCGOLlrZaGr5yyGyKdVaBwxW0F5iotQQsVBiU4sqKSZleH\nUbWy6KmQ5S92Gz6J4oAoIuvcd0ju1RAfbZuAo/VVAoGBAOPXGcUiDIonDKVPTB9l\nW7AYWGXC2qaSCzM5B0A6+Nb4ZqKs1zjtjT1a2JC+EHY9alFo2ccBvkjux8gQmPyX\nuR4wX6ynzUxYIzs6aTPbvxe6d8U8lGK1LpsgLFPxBb9BKIHQ9d62/QUOrb75n2/H\nDEJQGW+rrJ+d9XyEozpuIv+I\n-----END PRIVATE KEY-----" > "$certdir/odfe-node.key"
    echo -e "-----BEGIN CERTIFICATE-----\nMIIDdzCCAl+gAwIBAgIUDsi0qPmptSvcCCXXJ2Fm1pFmv0AwDQYJKoZIhvcNAQEL\nBQAwZTELMAkGA1UEBhMCVVMxEzARBgNVBAgMCldhc2hpbmd0b24xEDAOBgNVBAcM\nB1NlYXR0bGUxGjAYBgNVBAoMEUNoZWYgU29mdHdhcmUgSW5jMRMwEQYDVQQDDApj\naGVmcm9vdGNhMB4XDTIyMDYwMTA3MzMwMVoXDTI1MDUzMTA3MzMwMVowYzELMAkG\nA1UEBhMCVVMxEzARBgNVBAgMCldhc2hpbmd0b24xEDAOBgNVBAcMB1NlYXR0bGUx\nGjAYBgNVBAoMEUNoZWYgU29mdHdhcmUgSW5jMREwDwYDVQQDDAhjaGVmbm9kZTCC\nASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAM5iKA88y2M7T+OSD2vPTPFJ\nmSoKHEyZ3U8qW1ZL9ORY9c5yLpHazWIQ48S1Ad/OeggrrZG7JWOjBY1aYuXWgPe8\nZQ9nS3mUdEu3QjW52zJb5Tp8qO7MJGVXFNaBGoKDSvIuGpnlruNYLiwJbTj0mzkC\nRKvSXCg7iOszHt72VUkfRQM59iXh3LjNup6TPVMmW8Iamze7/NB9DMz4Ixjkknfc\nO9v1Fp2E+GJonpbIPBZGItBelMOH0g/EIBMASl3GX4+eBLndhcdjsdtzEV/fJE6H\nwgMtJGahb+17ztLK54dkZKQ7ES7WVlIq3Bfw+yepqaI6tnfIprqSaPclmeW+asUC\nAwEAAaMhMB8wHQYDVR0lBBYwFAYIKwYBBQUHAwIGCCsGAQUFBwMBMA0GCSqGSIb3\nDQEBCwUAA4IBAQBYhcyhj/PeVIcV7uWTK7lZxfEjEwG2Pydch+z8ElVuT8iYjuSE\nUMvJTjrX5topd+u9P1INUJAmjyXI8kJ42AoozCsH79iMwkX51O6Ri8GsAufDVeDf\nuTlNCxpnE2O1W1/l9UVUloTGW515KguPcC4xYdNwA6scN8rlhTtH8vvBteDUE+fq\nucRUikrbokEfgzHl3la0QlvQ8JnKprOAxK2eme815IK75ygpcLWKjveHFdC2VknY\nwbdG/4UzmsZbI2q5raa9nY3pSJ/kQ3IYg0lRyEAtIC1LsDw7aW1+Vlw5VR4arHRo\nLOEtF2OEtA1uREZpdtoViv7K36rTwDYwMDS+\n-----END CERTIFICATE-----" > "$certdir/odfe-node.pem"
    echo -e "-----BEGIN CERTIFICATE-----\nMIIDdzCCAl+gAwIBAgIUDsi0qPmptSvcCCXXJ2Fm1pFmv0AwDQYJKoZIhvcNAQEL\nBQAwZTELMAkGA1UEBhMCVVMxEzARBgNVBAgMCldhc2hpbmd0b24xEDAOBgNVBAcM\nB1NlYXR0bGUxGjAYBgNVBAoMEUNoZWYgU29mdHdhcmUgSW5jMRMwEQYDVQQDDApj\naGVmcm9vdGNhMB4XDTIyMDYwMTA3MzMwMVoXDTI1MDUzMTA3MzMwMVowYzELMAkG\nA1UEBhMCVVMxEzARBgNVBAgMCldhc2hpbmd0b24xEDAOBgNVBAcMB1NlYXR0bGUx\nGjAYBgNVBAoMEUNoZWYgU29mdHdhcmUgSW5jMREwDwYDVQQDDAhjaGVmbm9kZTCC\nASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAM5iKA88y2M7T+OSD2vPTPFJ\nmSoKHEyZ3U8qW1ZL9ORY9c5yLpHazWIQ48S1Ad/OeggrrZG7JWOjBY1aYuXWgPe8\nZQ9nS3mUdEu3QjW52zJb5Tp8qO7MJGVXFNaBGoKDSvIuGpnlruNYLiwJbTj0mzkC\nRKvSXCg7iOszHt72VUkfRQM59iXh3LjNup6TPVMmW8Iamze7/NB9DMz4Ixjkknfc\nO9v1Fp2E+GJonpbIPBZGItBelMOH0g/EIBMASl3GX4+eBLndhcdjsdtzEV/fJE6H\nwgMtJGahb+17ztLK54dkZKQ7ES7WVlIq3Bfw+yepqaI6tnfIprqSaPclmeW+asUC\nAwEAAaMhMB8wHQYDVR0lBBYwFAYIKwYBBQUHAwIGCCsGAQUFBwMBMA0GCSqGSIb3\nDQEBCwUAA4IBAQBYhcyhj/PeVIcV7uWTK7lZxfEjEwG2Pydch+z8ElVuT8iYjuSE\nUMvJTjrX5topd+u9P1INUJAmjyXI8kJ42AoozCsH79iMwkX51O6Ri8GsAufDVeDf\nuTlNCxpnE2O1W1/l9UVUloTGW515KguPcC4xYdNwA6scN8rlhTtH8vvBteDUE+fq\nucRUikrbokEfgzHl3la0QlvQ8JnKprOAxK2eme815IK75ygpcLWKjveHFdC2VknY\nwbdG/4UzmsZbI2q5raa9nY3pSJ/kQ3IYg0lRyEAtIC1LsDw7aW1+Vlw5VR4arHRo\nLOEtF2OEtA1uREZpdtoViv7K36rTwDYwMDS+\n-----END CERTIFICATE-----" > "$certdir/postgresql.pem"
    echo -e "-----BEGIN PRIVATE KEY-----\nMIIEvgIBADANBgkqhkiG9w0BAQEFAASCBKgwggSkAgEAAoIBAQDOYigPPMtjO0/j\nkg9rz0zxSZkqChxMmd1PKltWS/TkWPXOci6R2s1iEOPEtQHfznoIK62RuyVjowWN\nWmLl1oD3vGUPZ0t5lHRLt0I1udsyW+U6fKjuzCRlVxTWgRqCg0ryLhqZ5a7jWC4s\nCW049Js5AkSr0lwoO4jrMx7e9lVJH0UDOfYl4dy4zbqekz1TJlvCGps3u/zQfQzM\n+CMY5JJ33Dvb9RadhPhiaJ6WyDwWRiLQXpTDh9IPxCATAEpdxl+PngS53YXHY7Hb\ncxFf3yROh8IDLSRmoW/te87SyueHZGSkOxEu1lZSKtwX8PsnqamiOrZ3yKa6kmj3\nJZnlvmrFAgMBAAECggEBAJMQRtN00smHPko/jlWYho95jnbydW/wRE9hM2yu+3as\nHZxaqOyuKaQzvNoy0X8VtseTuD8cSGxTgmVTfx2gWI4v5SOwy4t7SdOaZse6BIRC\nD3o50nc3aMf94Y9ot+dAf2tLhxEbJiBCecRPg1EOnH1nLF/zqk/Q/KP87YabqfJL\n2hV/+ZbUtRzyC88X7zgfHOIPjKF6qZYj2rBF9d6J/nyk4+RGTQXuYMRXRfMa/+Vg\nPQA8vHLOF1ZGbDfLkATcRFy8bVaCHbESRJuVxx377mK3fsZJwP3en3dsANkurJFU\npf0hdW2rL+XoKyBgkd5ZC1XMW6nLBJkhnyuDkWSLwIECgYEA77vdAyXOBIrykR0P\nTnG7d79h3iccYtMEJt3+yK+i/z4xvJ4kUyujXfld+DC2EpudGvAfcPqswMcEfASU\nz7/S7Fjs1DoNqhzVQ2AbLtiXv6gDDqtxRhiQsRebDrCMsy3ak9S+qwx1po/GbAI4\n+Jyd1Ej7nFbr1jaRDE5v/NCkpWkCgYEA3GMAglhMyiiZIp1SCTyZlw/JsgeounW8\nKpCeJW6sRGeKhjU63K6JAKRVH8ZN4BCZAslHgyBlqFavRP1iNGz8aVzijuE3LYv9\ndjClbTKDeL5BVQCMZmMeHRx26A6C2VR3bCeogKCLG2p2w2e6aNyC/5vRswPQ9fHN\n9oQTdP+3Iv0CgYBhsaoARWCuQz1UW4ZITlq6Eti8Ii4UK6/uctsIR2LiJyM3nS5q\nYkch3aQopxkbfiTIvH/jl5Pcv32ZWHiYLLC0tgxU9A9renKCEWEidwtoA/yy9rSj\nfxE/ycHbe6Dqx83iHnlQ1TQAm2idzU4QKiq7Pr6iWF2lHvl/DVvfp9rguQKBgHeX\nNZCFr+3HiAmziu++Na42gAGejF3aQ6hpX5f6bXqVISNgwTc9ctSpWBPs/RdIim9r\nn+W079UgbO2n/eFq/rGCGOLlrZaGr5yyGyKdVaBwxW0F5iotQQsVBiU4sqKSZleH\nUbWy6KmQ5S92Gz6J4oAoIuvcd0ju1RAfbZuAo/VVAoGBAOPXGcUiDIonDKVPTB9l\nW7AYWGXC2qaSCzM5B0A6+Nb4ZqKs1zjtjT1a2JC+EHY9alFo2ccBvkjux8gQmPyX\nuR4wX6ynzUxYIzs6aTPbvxe6d8U8lGK1LpsgLFPxBb9BKIHQ9d62/QUOrb75n2/H\nDEJQGW+rrJ+d9XyEozpuIv+I\n-----END PRIVATE KEY-----" > "$certdir/postgresql.key"
    chmod 777 "$certdir"/*

    docker cp "$HA_BACKEND_DIR/setup.sh" "${ha_backend_container1}:/setup.sh"
    docker cp "$HA_BACKEND_DIR/setup.sh" "${ha_backend_container2}:/setup.sh"
    docker cp "$certdir" "${ha_backend_container1}:/certificates"
    docker cp "$certdir" "${ha_backend_container2}:/certificates"


    docker exec -t "$ha_backend_container1" /setup.sh "$ha_backend_container1_ip"
    docker exec -t "$ha_backend_container2" /setup.sh "$ha_backend_container2_ip"

    local errcode
    local output
    for try in {1..60}; do
        echo "Trying to create dbuser (attempt #${try})"
        errcode="0"
        output="$(docker exec --env PGPASSWORD="$ha_admin_pg_password" --env HAB_LICENSE=accept-no-persist "$ha_backend_container1" \
	hab pkg exec core/postgresql13 -- psql \
            -h 127.0.0.1 -p 7432 -U admin -d postgres -c \
            "CREATE USER dbuser WITH PASSWORD '$ha_admin_pg_password'")" || errcode="$?"
        if [ "$errcode" -eq "0" ]; then
            break
        else
            echo "Retrying in 5 seconds"
            sleep 5
        fi
    done

    if [ ! "$errcode" -eq "0" ]; then
        echo "Failed to create dbuser($errcode): $output"
        return 1
    fi

    cat <<DOC > "$ha_backend_config"
[global.v1.external.opensearch]
enable = true
nodes = ["https://${ha_backend_container1_ip}:9200", "https://${ha_backend_container2_ip}:9200"]

[global.v1.external.opensearch.backup]
enable = true
location = "fs"

[global.v1.external.opensearch.backup.fs]
path = "/services/ha_backend_backups"

[global.v1.external.opensearch.auth]
scheme = "basic_auth"

[global.v1.external.opensearch.auth.basic_auth]
username = "${HA_BACKEND_USER}"
password = "${HA_BACKEND_PASSWORD}"

[global.v1.external.opensearch.ssl]
# defaults from automate-ha-backend
server_name = "chefnode"
root_cert = """$(cat "${certdir}/MyRootCA.pem")"""

[global.v1.external.postgresql]
enable = true
nodes = ["${ha_backend_container1_ip}:7432", "${ha_backend_container2_ip}:7432"]

[global.v1.external.postgresql.ssl]
enable = true
root_cert = """$(cat "${certdir}/MyRootCA.pem")"""

[global.v1.external.postgresql.auth]
scheme = "password"

[global.v1.external.postgresql.auth.password.superuser]
username = "admin"
password = "$ha_admin_pg_password"
[global.v1.external.postgresql.auth.password.dbuser]
username = "dbuser"
password = "$ha_admin_pg_password"

[global.v1.external.postgresql.backup]
enable = true

[esgateway.v1.sys.service]
host = "0.0.0.0"
DOC

    for try in {1..8}; do
        echo "Testing odfe connectivity (attempt ${try})"

        errcode=0
        curl --connect-timeout 15 --max-time 15 -u admin:admin --cacert "${certdir}/MyRootCA.pem" --key "${certdir}/odfe-admin.key" --cert "${certdir}/odfe-admin.pem" --resolve "chefnode:9200:${ha_backend_container1_ip}" "https://chefnode:9200" &&
        curl --connect-timeout 15 --max-time 15 -u admin:admin --cacert "${certdir}/MyRootCA.pem" --key "${certdir}/odfe-admin.key" --cert "${certdir}/odfe-admin.pem" --resolve "chefnode:9200:${ha_backend_container2_ip}" "https://chefnode:9200" || errcode=${?}
        if [ ${errcode} -eq 0 ]; then
            break
        else
            if [ ${try} -eq 8 ]; then
                echo "Failed to validate odfe connectivity"
                return 1
            fi
            sleep 15
        fi
    done
}

ha_backend_dump_logs() {
    tmpdir=$(mktemp -d)
    docker exec -t "$ha_backend_container1" journalctl --no-pager -u hab-sup > "$tmpdir/ha_backend_container_1"
    docker exec -t "$ha_backend_container2" journalctl --no-pager -u hab-sup > "$tmpdir/ha_backend_container_2"

    if command -v buildkite-agent; then
        if ! buildkite-agent artifact upload "$tmpdir/*"
        then
            echo "Failed to ha backend container logs"
        fi
    fi
    rm -r "$tmpdir"
}

ha_backend_teardown() {
    docker stop "$ha_backend_container1"
    docker stop "$ha_backend_container2"
}
